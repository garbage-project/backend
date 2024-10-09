package com.project.trash.facility.request;

import java.math.BigDecimal;
import java.util.Set;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;

@Getter
public class FacilityEntryRequest {

  @Schema(description = "시설물 종류 목록(R - 화장실, S - 흡연구역, T - 쓰레기통)", example = "R")
  private String type;

  @Schema(description = "시설물명", example = "쌍문역 내 화장실")
  private String name;

  @Schema(description = "위치", example = "쌍문역")
  private String location;

  @Schema(description = "상세 위치", example = "지하 1층")
  private String detailLocation;

  @Schema(description = "위도", example = "37.413294")
  private BigDecimal latitude;

  @Schema(description = "경도", example = "126.764166")
  private BigDecimal longitude;

  @Schema(description = "정보", example = "개찰구 내에 존재합니다.", nullable = true)
  private String information;

  @Schema(description = "이미지 ID 목록(최대 3개)", example = "[1, 2]", nullable = true)
  private Set<Long> imageIds;
}
