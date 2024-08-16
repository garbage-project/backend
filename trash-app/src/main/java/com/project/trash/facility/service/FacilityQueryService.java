package com.project.trash.facility.service;

import com.project.trash.facility.dao.FacilityDao;
import com.project.trash.facility.request.FacilityListRequest;
import com.project.trash.facility.response.FacilityListResponse;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

import lombok.RequiredArgsConstructor;

/**
 * 시설물 조회 서비스
 */
@Service
@RequiredArgsConstructor
public class FacilityQueryService {

  private final FacilityDao facilityDao;

  /**
   * 시설물 목록 조회
   */
  @Transactional(readOnly = true)
  public List<FacilityListResponse> getList(FacilityListRequest param) {
    return facilityDao.select(param);
  }
}
