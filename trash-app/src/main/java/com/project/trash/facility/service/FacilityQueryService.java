package com.project.trash.facility.service;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.facility.dao.FacilityDao;
import com.project.trash.facility.domain.Facility;
import com.project.trash.facility.repository.FacilityRepository;
import com.project.trash.facility.request.FacilityListRequest;
import com.project.trash.facility.response.FacilityListResponse;
import com.project.trash.member.response.MyFacilityListResponse;

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

  /**
   * 시설물 목록 조회
   */
  @Transactional(readOnly = true)
  public List<FacilityListResponse> getList(FacilityListRequest param) {
    return facilityDao.select(param);
  }

  /**
   * 등록한 시설물 목록 조회
   */
  @Transactional(readOnly = true)
  public List<MyFacilityListResponse> getList() {
    return facilityDao.select();
  }

  /**
   * 시설물 단일 조회
   */
  @Transactional(readOnly = true)
  public Facility getOne(Long facilitySeq, Long memberSeq) {
    return facilityRepository.findByFacilitySeqAndMemberSeq(facilitySeq, memberSeq)
                             .orElseThrow(() -> new ValidationException(FACILITY_NOT_FOUND));
  }
}
