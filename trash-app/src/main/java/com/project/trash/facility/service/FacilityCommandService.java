package com.project.trash.facility.service;

import com.project.trash.aws.dao.AwsFileDao;
import com.project.trash.common.domain.enums.ImageType;
import com.project.trash.common.exception.ValidationException;
import com.project.trash.common.response.ImageEntryResponse;
import com.project.trash.facility.domain.Facility;
import com.project.trash.facility.domain.FacilityImage;
import com.project.trash.facility.domain.enums.FacilityType;
import com.project.trash.facility.repository.FacilityImageRepository;
import com.project.trash.facility.repository.FacilityRepository;
import com.project.trash.facility.request.FacilityEntryRequest;
import com.project.trash.facility.request.FacilityModifyRequest;
import com.project.trash.utils.MemberUtils;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.util.ArrayList;
import java.util.List;
import java.util.Set;

import lombok.RequiredArgsConstructor;

import static com.project.trash.common.domain.resultcode.FacilityResultCode.FACILITY_IMAGE_NOT_FOUND;
import static com.project.trash.common.domain.resultcode.RequestResultCode.PARAM_INVALID;

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
   * 시설물 삭제
   */
  @Transactional
  public void delete(Long facilityId) {
    Facility facility = facilityQueryService.getOne(facilityId, MemberUtils.getMemberId());

    facilityRepository.delete(facility);
  }

  /**
   * 시설물 등록
   */
  @Transactional
  public void entry(FacilityEntryRequest param) {
    Facility facility = facilityRepository.save(new Facility(FacilityType.fromCode(param.getType()), param.getName(), param.getLocation(),
        param.getDetailLocation(), param.getLatitude(), param.getLongitude(),
        param.getInformation(), String.valueOf(MemberUtils.getMemberId())));

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
  public ImageEntryResponse entry(List<MultipartFile> images) {
    Long memberId = MemberUtils.getMemberId();
    List<FacilityImage> facilityImages = images
        .stream()
        .map(image -> new FacilityImage(awsFileDao.upload(memberId, ImageType.FACILITY.getType(), image)))
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
    Facility facility = facilityQueryService.getOne(param.getFacilityId(), MemberUtils.getMemberId());

    facility.update(FacilityType.fromCode(param.getType()), param.getName(), param.getLocation(),
        param.getDetailLocation(), param.getLatitude(), param.getLongitude(),
        param.getInformation());

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
}
