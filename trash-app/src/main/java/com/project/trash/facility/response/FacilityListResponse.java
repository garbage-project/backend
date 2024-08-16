package com.project.trash.facility.response;

import org.bson.types.Decimal128;
import org.springframework.data.annotation.Id;

import java.math.BigDecimal;

import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * 시설물 목록 응답
 */
@Getter
@Setter
@NoArgsConstructor
public class FacilityListResponse {

  /**
   * 시설물 ID
   */
  @Id
  private String facilityId;
  /**
   * 시설물 종류
   */
  private String type;
  /**
   * 위도
   */
  private BigDecimal latitude;
  /**
   * 경도
   */
  private BigDecimal longitude;

  public FacilityListResponse(String facilityId, String type, Decimal128 latitude, Decimal128 longitude) {
    this.facilityId = facilityId;
    this.type = type;
    this.latitude = latitude.bigDecimalValue();
    this.longitude = longitude.bigDecimalValue();
  }
}
