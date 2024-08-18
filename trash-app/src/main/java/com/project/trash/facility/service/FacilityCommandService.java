package com.project.trash.facility.service;

import com.project.trash.aws.dao.AwsFileDao;
import com.project.trash.common.domain.enums.ImageType;
import com.project.trash.facility.domain.Facility;
import com.project.trash.facility.domain.enums.FacilityType;
import com.project.trash.facility.repository.FacilityRepository;
import com.project.trash.facility.request.FacilityEntryRequest;
import com.project.trash.utils.MemberUtils;

import org.bson.types.Decimal128;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;

import lombok.RequiredArgsConstructor;

/**
 * 시설물 등록/수정/삭제 서비스
 */
@Service
@RequiredArgsConstructor
public class FacilityCommandService {

  private final FacilityRepository facilityRepository;
  private final AwsFileDao awsFileDao;

  /**
   * 시설물 등록
   */
  @Transactional
  public void entry(FacilityEntryRequest param, List<MultipartFile> fileList) {
    facilityRepository.save(
        new Facility(FacilityType.fromCode(param.getType()), param.getLocation(), param.getDetailLocation(),
            new Decimal128(param.getLatitude()), new Decimal128(param.getLongitude()), param.getInformation(),
            makeImageList(fileList), MemberUtils.getMemberSeq()));
  }

  private List<String> makeImageList(List<MultipartFile> imageList) {
    if (imageList == null) {
      return null;
    }
    Long memberSeq = MemberUtils.getMemberSeq();
    return imageList.stream().map(file -> awsFileDao.upload(memberSeq, ImageType.FACILITY.getType(), file)).toList();
  }
}
