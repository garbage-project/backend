package com.project.trash.facility.service;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.facility.dao.FacilityDao;
import com.project.trash.facility.domain.Facility;
import com.project.trash.facility.repository.FacilityRepository;
import com.project.trash.facility.request.FacilityListRequest;
import com.project.trash.facility.response.FacilityDetailResponse;
import com.project.trash.facility.response.FacilityListResponse;
import com.project.trash.member.request.MemberFacilityListRequest;
import com.project.trash.member.response.MemberDetailResponse;
import com.project.trash.member.response.MemberFacilityListResponse;
import com.project.trash.member.service.MemberQueryService;

import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

import lombok.RequiredArgsConstructor;

import static com.project.trash.common.domain.resultcode.FacilityResultCode.FACILITY_NOT_FOUND;

/**
 * 시설물 조회 서비스
 */
@Service
@RequiredArgsConstructor
public class FacilityQueryService {

  @Value("${cloud.aws.s3.url}")
  private String s3ImageUrl;

  private final FacilityDao facilityDao;
  private final FacilityRepository facilityRepository;

  private final MemberQueryService memberQueryService;

  /**
   * 등록한 시설물 목록 조회
   */
  @Transactional(readOnly = true)
  public Pair<List<MemberFacilityListResponse>, Long> getList(MemberFacilityListRequest param) {
    // 회원 존재여부 검증
    memberQueryService.verifyExist(param.getMemberId());

    return Pair.of(facilityDao.select(param), facilityDao.count(param.getMemberId()));
  }

  /**
   * 시설물 상세 조회
   */
  @Transactional(readOnly = true)
  public FacilityDetailResponse getDetail(Long facilityId) {
    return new FacilityDetailResponse(getOne(facilityId), s3ImageUrl);
  }

  /**
   * 시설물 목록 조회
   */
  @Transactional(readOnly = true)
  public Pair<List<FacilityListResponse>, Long> getList(FacilityListRequest param) {
    return Pair.of(facilityDao.select(param), facilityDao.count(param));
  }

  /**
   * 시설물 단일 조회
   */
  @Transactional(readOnly = true)
  public Facility getOne(Long facilityId) {
    return facilityRepository.findById(facilityId).orElseThrow(() -> new ValidationException(FACILITY_NOT_FOUND));
  }
}