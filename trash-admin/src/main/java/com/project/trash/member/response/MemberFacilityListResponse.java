package com.project.trash.member.response;

import org.jooq.types.ULong;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import trash.tables.records.FacilityRecord;

/**
 * 등록한 시설물 목록 응답
 */
@Getter
@Schema(title = "회원이 등록한 시설물 목록 조회 응답")
public class MemberFacilityListResponse {

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
   * 시설물명
   */
  @Schema(description = "시설물명", example = "쌍문역 내 화장실")
  private String name;
  /**
   * 위치
   */
  @Schema(description = "위치", example = "쌍문역")
  private String location;
  /**
   * 상세 위치
   */
  @Schema(description = "상세 위치", example = "지하 1층")
  private String detailLocation;
  /**
   * 정보
   */
  @Schema(description = "정보", example = "개찰구 내에 존재합니다.", nullable = true)
  private String information;
  /**
   * 승인 상태
   */
  @Schema(description = "승인 상태(P - 승인대기, A - 승인완료, R - 승인거절, S - 승인중단)", example = "A")
  private String approvalStatus;
  /**
   * 승인일자
   */
  @Schema(description = "승인일자 (yyyy-MM-dd)", example = "2024-09-01")
  private String approvalDate;

  public MemberFacilityListResponse(ULong facilitySeq, String type, String name, String location, String detailLocation,
      String information, String approvalStatus) {
    this.facilitySeq = facilitySeq;
    this.type = type;
    this.name = name;
    this.location = location;
    this.detailLocation = detailLocation;
    this.information = information;
    this.approvalStatus = approvalStatus;
  }
}