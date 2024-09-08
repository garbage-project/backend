package com.project.trash.facility.service;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.facility.dao.FacilityDao;
import com.project.trash.facility.domain.Facility;
import com.project.trash.facility.repository.FacilityRepository;
import com.project.trash.member.request.MemberFacilityListRequest;
import com.project.trash.member.response.MemberFacilityListResponse;
import com.project.trash.member.service.MemberQueryService;

import org.apache.commons.lang3.tuple.Pair;
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

  private final FacilityDao facilityDao;
  private final FacilityRepository facilityRepository;

  private final MemberQueryService memberQueryService;

  /**
   * 등록한 시설물 목록 조회
   */
  @Transactional(readOnly = true)
  public Pair<List<MemberFacilityListResponse>, Long> getList(MemberFacilityListRequest param) {
    // 회원 존재여부 검증
    memberQueryService.verifyExist(param.getMemberSeq());

    return Pair.of(facilityDao.select(param), facilityDao.count(param.getMemberSeq()));
  }

  /**
   * 시설물 단일 조회
   */
  @Transactional(readOnly = true)
  public Facility getOne(String facilityId) {
    return facilityRepository.findById(facilityId).orElseThrow(() -> new ValidationException(FACILITY_NOT_FOUND));
  }
}