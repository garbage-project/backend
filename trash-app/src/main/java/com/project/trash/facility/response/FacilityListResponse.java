package com.project.trash.facility.response;

import org.jooq.types.ULong;

import java.math.BigDecimal;
import java.util.List;

import io.swagger.v3.oas.annotations.Parameter;
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

  @Schema(description = "남쪽 위도", example = "37.413294")
  private BigDecimal southLatitude;

  @Schema(description = "북쪽 위도", example = "37.701749")
  private BigDecimal northLatitude;

  @Schema(description = "서쪽 경도", example = "126.764166")
  private BigDecimal westLongitude;

  @Schema(description = "동쪽 경도", example = "127.181111")
  private BigDecimal eastLongitude;

  @Schema(description = "시설물 목록")
  private List<FacilityList> list;

  public FacilityListResponse(BigDecimal southLatitude, BigDecimal northLatitude, BigDecimal westLongitude,
      BigDecimal eastLongitude, List<FacilityList> list) {
    this.southLatitude = southLatitude;
    this.northLatitude = northLatitude;
    this.westLongitude = westLongitude;
    this.eastLongitude = eastLongitude;
    this.list = list;
  }

  @Getter
  @Setter
  public static class FacilityList {

    @Schema(description = "시설물 ID", example = "1")
    private ULong facilityId;

    @Schema(description = "시설물 종류 목록(R - 화장실, S - 흡연구역, T - 쓰레기통)", example = "R")
    private String type;

    @Schema(description = "위도", example = "37.413294")
    private BigDecimal latitude;

    @Schema(description = "경도", example = "126.764166")
    private BigDecimal longitude;

    public FacilityList(ULong facilityId, String type, BigDecimal latitude, BigDecimal longitude) {
      this.facilityId = facilityId;
      this.type = type;
      this.latitude = latitude;
      this.longitude = longitude;
    }
  }
}
