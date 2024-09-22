package com.project.trash.facility.service;

import com.project.trash.common.exception.ValidationException;
import com.project.trash.facility.dao.FacilityDao;
import com.project.trash.facility.domain.Facility;
import com.project.trash.facility.domain.enums.FacilityApprovalStatus;
import com.project.trash.facility.repository.FacilityRepository;
import com.project.trash.facility.request.FacilityListRequest;
import com.project.trash.facility.response.FacilityDetailResponse;
import com.project.trash.facility.response.FacilityListResponse;
import com.project.trash.member.response.MyFacilityListResponse;

import org.apache.commons.lang3.tuple.Pair;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.math.RoundingMode;
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

  /**
   * 시설물 목록 조회
   */
  @Transactional(readOnly = true)
  public FacilityListResponse getList(FacilityListRequest param) {
    List<FacilityListResponse.FacilityList> list = facilityDao.select(param);

    BigDecimal KM_IN_ONE_DEGREE_LAT = BigDecimal.valueOf(111.32); // 위도 1도에 해당하는 거리 (약 111.32km)
    BigDecimal distanceInKm = BigDecimal.valueOf(25.0);  // 25km

    // 남북으로 25km 떨어진 위도 계산
    BigDecimal latitude = param.getLatitude();
    BigDecimal longitude = param.getLongitude();
    BigDecimal northLatitude = latitude.add(distanceInKm.divide(KM_IN_ONE_DEGREE_LAT, 10, RoundingMode.HALF_UP));
    BigDecimal southLatitude = latitude.subtract(distanceInKm.divide(KM_IN_ONE_DEGREE_LAT, 10, RoundingMode.HALF_UP));

    // 동서로 25km 떨어진 경도 계산 (위도의 영향을 받음)
    BigDecimal cosLatitude = BigDecimal.valueOf(Math.cos(Math.toRadians(latitude.doubleValue())));
    BigDecimal kmPerDegreeLongitude = KM_IN_ONE_DEGREE_LAT.multiply(cosLatitude);
    BigDecimal eastLongitude = longitude.add(distanceInKm.divide(kmPerDegreeLongitude, 10, RoundingMode.HALF_UP));
    BigDecimal westLongitude = longitude.subtract(distanceInKm.divide(kmPerDegreeLongitude, 10, RoundingMode.HALF_UP));

    return new FacilityListResponse(northLatitude, southLatitude, westLongitude, eastLongitude, list);
  }

  /**
   * 등록한 시설물 목록 조회
   */
  @Transactional(readOnly = true)
  public List<MyFacilityListResponse> getList() {
    return facilityDao.select();
  }

  /**
   * 시설물 상세 조회
   */
  @Transactional(readOnly = true)
  public FacilityDetailResponse getDetail(Long facilityId) {
    return new FacilityDetailResponse(getOne(facilityId), s3ImageUrl);
  }

  @Transactional(readOnly = true)
  public Facility getOne(Long facilityId, Long memberId) {
    return facilityRepository.findByFacilityIdAndMemberId(facilityId, String.valueOf(memberId))
                             .orElseThrow(() -> new ValidationException(FACILITY_NOT_FOUND));
  }

  @Transactional(readOnly = true)
  public Facility getOne(Long facilityId) {
    return facilityRepository.findByFacilityIdAndApprovalStatus(facilityId, FacilityApprovalStatus.APPROVE)
                             .orElseThrow(() -> new ValidationException(FACILITY_NOT_FOUND));
  }
}
