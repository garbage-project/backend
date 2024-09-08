package com.project.trash.facility.response;

import org.jooq.types.ULong;

import java.math.BigDecimal;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;

/**
 * 시설물 목록 응답
 */
@Getter
@Setter
@Schema(title = "시설물 목록 조회 응답")
public class FacilityListResponse {

  /**
   * 시설물 일련번호
   */
  @Schema(description = "시설물 일련번호", example = "1")
  private ULong facilitySeq;
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

  public FacilityListResponse(ULong facilitySeq, String type, BigDecimal latitude, BigDecimal longitude) {
    this.facilitySeq = facilitySeq;
    this.type = type;
    this.latitude = latitude;
    this.longitude = longitude;
  }
}
