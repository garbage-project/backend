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

  @Schema(description = "시설물 ID", example = "1")
  private ULong facilityId;

  @Schema(description = "시설물 종류 목록(R - 화장실, S - 흡연구역, T - 쓰레기통)", example = "R")
  private String type;

  @Schema(description = "위도", example = "37.413294")
  private BigDecimal latitude;

  @Schema(description = "경도", example = "126.764166")
  private BigDecimal longitude;

  public FacilityListResponse(ULong facilityId, String type, BigDecimal latitude, BigDecimal longitude) {
    this.facilityId = facilityId;
    this.type = type;
    this.latitude = latitude;
    this.longitude = longitude;
  }
}
