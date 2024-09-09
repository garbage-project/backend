package com.project.trash.facility.service;

import com.project.trash.aws.dao.AwsFileDao;
import com.project.trash.common.domain.enums.ImageType;
import com.project.trash.common.exception.ValidationException;
import com.project.trash.facility.domain.Facility;
import com.project.trash.facility.domain.enums.FacilityType;
import com.project.trash.facility.repository.FacilityRepository;
import com.project.trash.facility.request.FacilityEntryRequest;
import com.project.trash.facility.request.FacilityModifyRequest;
import com.project.trash.utils.MemberUtils;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.util.ArrayList;
import java.util.List;

import lombok.RequiredArgsConstructor;

import static com.project.trash.common.domain.resultcode.RequestResultCode.PARAM_INVALID;

/**
 * 시설물 등록/수정/삭제 서비스
 */
@Service
@RequiredArgsConstructor
public class FacilityCommandService {

  private final FacilityRepository facilityRepository;
  private final FacilityQueryService facilityQueryService;
  private final AwsFileDao awsFileDao;

  /**
   * 시설물 삭제
   */
  @Transactional
  public void delete(Long facilitySeq) {
    Facility facility = facilityQueryService.getOne(facilitySeq, MemberUtils.getMemberSeq());

    facilityRepository.delete(facility);
  }

  /**
   * 시설물 등록
   */
  @Transactional
  public void entry(FacilityEntryRequest param) {
    facilityRepository.save(new Facility(FacilityType.fromCode(param.getType()), param.getName(), param.getLocation(),
        param.getDetailLocation(), param.getLatitude(), param.getLongitude(),
        param.getInformation(), String.valueOf(MemberUtils.getMemberSeq())));
  }

  /**
   * 시설물 수정
   */
  @Transactional
  public void modify(FacilityModifyRequest param) {
    Facility facility = facilityQueryService.getOne(param.getFacilitySeq(), MemberUtils.getMemberSeq());

    facility.update(FacilityType.fromCode(param.getType()), param.getName(), param.getLocation(),
        param.getDetailLocation(), param.getLatitude(), param.getLongitude(),
        param.getInformation());

    facilityRepository.save(facility);
  }

  /**
   * 이미지 추가
   */
  private List<String> makeImages(List<MultipartFile> addImages) {
    if (addImages == null) {
      return null;
    }
    Long memberSeq = MemberUtils.getMemberSeq();
    return addImages.stream().map(file -> awsFileDao.upload(memberSeq, ImageType.FACILITY.getType(), file)).toList();
  }

  /**
   * 이미지 추가 및 삭제
   */
  private List<String> modifyImages(List<String> savedImages, List<Integer> imageIndexes,
      List<MultipartFile> addImages) {
    if (imageIndexes == null && addImages == null) {
      return null;
    }

    List<String> images = new ArrayList<>();
    if (imageIndexes != null) {
      for (int index : imageIndexes) {
        if (index >= 0 && index < savedImages.size()) {
          images.add(savedImages.get(index));
        } else {
          throw new ValidationException(PARAM_INVALID);
        }
      }
    }

    if (addImages == null) {
      return images;
    }
    Long memberSeq = MemberUtils.getMemberSeq();
    addImages.forEach(file -> images.add(awsFileDao.upload(memberSeq, ImageType.FACILITY.getType(), file)));
    System.out.println("size: " + images.size());
    return images;
  }
}
