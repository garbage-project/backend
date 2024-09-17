package com.project.trash.facility.service;

import com.project.trash.common.aws.dao.AwsFileDao;
import com.project.trash.common.domain.enums.ImageType;
import com.project.trash.common.exception.ValidationException;
import com.project.trash.common.response.ImageEntryResponse;
import com.project.trash.facility.domain.Facility;
import com.project.trash.facility.domain.FacilityImage;
import com.project.trash.facility.domain.enums.FacilityApprovalStatus;
import com.project.trash.facility.domain.enums.FacilityType;
import com.project.trash.facility.repository.FacilityImageRepository;
import com.project.trash.facility.repository.FacilityRepository;
import com.project.trash.facility.request.FacilityEntryRequest;
import com.project.trash.facility.request.FacilityModifyRequest;
import com.project.trash.utils.AdminUtils;

import org.apache.commons.io.FilenameUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.poi.hssf.usermodel.HSSFWorkbook;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.CellType;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Sheet;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.security.core.parameters.P;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.io.IOException;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import lombok.RequiredArgsConstructor;

import static com.project.trash.common.domain.resultcode.FacilityResultCode.FACILITY_EXCEL_EXTENSION_INVALID;
import static com.project.trash.common.domain.resultcode.FacilityResultCode.FACILITY_EXCEL_READ_FAIL;
import static com.project.trash.common.domain.resultcode.FacilityResultCode.FACILITY_IMAGE_NOT_FOUND;

/**
 * 시설물 등록/수정/삭제 서비스
 */
@Service
@RequiredArgsConstructor
public class FacilityCommandService {

  private final FacilityRepository facilityRepository;
  private final FacilityQueryService facilityQueryService;

  private final FacilityImageRepository facilityImageRepository;

  private final AwsFileDao awsFileDao;

  /**
   * 선택한 시설물 목록 삭제
   */
  @Transactional
  public void delete(Set<Long> facilityIds) {
    facilityRepository.deleteAllByIdInBatch(facilityIds);
  }

  /**
   * 시설물 등록
   */
  @Transactional
  public void entry(FacilityEntryRequest param) {
    Facility facility = facilityRepository.save(new Facility(FacilityType.fromCode(param.getType()), param.getName(), param.getLocation(),
        param.getDetailLocation(), param.getLatitude(), param.getLongitude(),
        param.getInformation(), AdminUtils.getId()));

    Set<Long> addImageIds = param.getImageIds();
    if (addImageIds != null && !addImageIds.isEmpty()) {
      List<FacilityImage> addImages = facilityImageRepository.findAllById(param.getImageIds());
      if (addImages.size() != addImageIds.size()) {
        throw new ValidationException(FACILITY_IMAGE_NOT_FOUND);
      }
      for (FacilityImage image : addImages) {
        image.setFacility(facility);
      }
    }
  }

  /**
   * 시설물 이미지 등록
   */
  @Transactional
  public ImageEntryResponse entryImages(List<MultipartFile> images) {
    String adminId = AdminUtils.getId();
    List<FacilityImage> facilityImages = images
        .stream()
        .map(image -> new FacilityImage(awsFileDao.upload(adminId, ImageType.FACILITY.getType(), image)))
        .toList();

    return new ImageEntryResponse(facilityImageRepository.saveAll(facilityImages)
        .stream()
        .map(FacilityImage::getImgId)
        .toList());
  }

  /**
   * 시설물 수정
   */
  @Transactional
  public void modify(FacilityModifyRequest param) {
    Facility facility = facilityQueryService.getOne(param.getFacilityId());

    facility.update(FacilityType.fromCode(param.getType()), param.getName(), param.getLocation(),
        param.getDetailLocation(), param.getLatitude(), param.getLongitude(), param.getInformation(),
        param.getDepartment(), param.getDepartmentPhoneNumber(), FacilityApprovalStatus.fromCode(param.getApprovalStatus()));

    modifyImages(facility, param.getImageIds());
  }

  private void modifyImages(Facility facility, Set<Long> addImageIds) {
    List<FacilityImage> savedImages = facility.getImages();
    if (addImageIds != null && !addImageIds.isEmpty()) {
      for (FacilityImage image : savedImages) {
        if (!addImageIds.contains(image.getImgId())) {
          image.setFacility(null);
        } else {
          addImageIds.remove(image.getImgId());
        }
      }
      List<FacilityImage> addImages = facilityImageRepository.findAllById(addImageIds);
      if (addImages.size() != addImageIds.size()) {
        throw new ValidationException(FACILITY_IMAGE_NOT_FOUND);
      }
      for (FacilityImage image : addImages) {
        image.setFacility(facility);
      }
    } else {
      // 시설물의 모든 이미지 제거
      for (FacilityImage image : savedImages) {
        image.setFacility(null);
      }
    }
  }

  public void entry(MultipartFile file, FacilityType type) {
    String extension = FilenameUtils.getExtension(file.getOriginalFilename());

    if (StringUtils.isBlank(extension) || (!extension.equals("xlsx") && !extension.equals("xls"))) {
      throw new ValidationException(FACILITY_EXCEL_EXTENSION_INVALID);
    }

    Workbook workbook;

    try {
      if (extension.equals("xlsx")) {
        workbook = new XSSFWorkbook(file.getInputStream());
      } else {
        workbook = new HSSFWorkbook(file.getInputStream());
      }

      Sheet worksheet = workbook.getSheetAt(0);
      List<Facility> addFacilities = new ArrayList<>();
      for (int i = 1; i < worksheet.getPhysicalNumberOfRows(); i++) {
        Row row = worksheet.getRow(i);

        try {
          String name = String.valueOf(getCellValue(row.getCell(0)));
          String location = String.valueOf(getCellValue(row.getCell(1)));
          String detailLocation = String.valueOf(getCellValue(row.getCell(2)));
          BigDecimal latitude = (BigDecimal) getCellValue(row.getCell(3));
          BigDecimal longitude = (BigDecimal) getCellValue(row.getCell(4));
          String department = String.valueOf(getCellValue(row.getCell(5)));
          String departmentPhoneNumber = String.valueOf(getCellValue(row.getCell(6)));

          if (name == null || location == null || latitude == null || longitude == null) {
            continue;
          }

          Facility facility = new Facility(name, location, detailLocation, latitude,
              longitude, department, departmentPhoneNumber, type);

          addFacilities.add(facility);
        } catch (Exception e) {
        }
      }

      facilityRepository.saveAll(addFacilities);
    } catch (IOException e) {
      throw new ValidationException(FACILITY_EXCEL_READ_FAIL);
    }
  }

  private Object getCellValue(Cell cell) {
    if (cell == null) {
      return null;
    }

    return switch (cell.getCellType()) {
      case STRING -> {
        String str = cell.getStringCellValue();
        yield StringUtils.isBlank(str) || str.equals("-") ? null : str;
      }
      case NUMERIC -> BigDecimal.valueOf(cell.getNumericCellValue());
      case BLANK -> null;
      default -> throw new ValidationException(FACILITY_EXCEL_READ_FAIL);
    };
  }
}