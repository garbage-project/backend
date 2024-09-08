package com.project.trash.facility.response;

import org.bson.types.Decimal128;
import org.springframework.data.annotation.Id;

import java.math.BigDecimal;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

/**
 * 시설물 목록 응답
 */
@Getter
@Setter
@NoArgsConstructor
@Schema(title = "시설물 목록 조회 응답")
public class FacilityListResponse {

  /**
   * 시설물 ID
   */
  @Id
  @Schema(description = "시설물 ID", example = "66c3194180a12933dd772938")
  private String facilityId;
  /**
   * 시설물 종류
   */
  @Schema(description = "시설물 종류 목록(R - 화장실, S - 흡연구역, T - 쓰레기통)", example = "R")
  private String type;
  /**
   * 위도
   */
  @Schema(description = "위도", example = "2.6112336")
  private BigDecimal latitude;
  /**
   * 경도
   */
  @Schema(description = "경도", example = "33.126115")
  private BigDecimal longitude;

  public FacilityListResponse(String facilityId, String type, Decimal128 latitude, Decimal128 longitude) {
    this.facilityId = facilityId;
    this.type = type;
    this.latitude = latitude.bigDecimalValue();
    this.longitude = longitude.bigDecimalValue();
  }
}
