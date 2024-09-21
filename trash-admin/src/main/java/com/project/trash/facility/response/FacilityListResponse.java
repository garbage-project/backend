package com.project.trash.facility.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.project.trash.common.utils.DateTimeUtils;

import org.jooq.types.ULong;

import java.math.BigDecimal;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import trash.tables.records.FacilityRecord;

/**
 * 시설물 목록 응답
 */
@Getter
@Setter
@Schema(title = "시설물 목록 조회 응답")
@JsonInclude(JsonInclude.Include.NON_NULL)
public class FacilityListResponse {

  @Schema(description = "시설물 ID", example = "1")
  private ULong facilityId;

  @Schema(description = "시설물 종류 (R - 화장실, S - 흡연구역, T - 쓰레기통)", example = "R")
  private String type;

  @Schema(description = "시설물명", example = "쌍문역 내 화장실")
  private String name;

  @Schema(description = "위치", example = "쌍문역")
  private String location;

  @Schema(description = "상세 위치", example = "지하 1층")
  private String detailLocation;

  @Schema(description = "정보", example = "개찰구 내에 존재합니다.", nullable = true)
  private String information;

  @Schema(description = "관리 부서", example = "서울시설공단")
  private String department;

  @Schema(description = "관리 부서 전화번호", example = "02-2290-7111")
  private String departmentPhoneNumber;

  @Schema(description = "승인 상태 (P - 승인대기, A - 승인완료, R - 승인거절, S - 승인중단)", example = "A")
  private String approvalStatus;

  @Schema(description = "등록자 ID", example = "test1234")
  private String memberId;

  @Schema(description = "시설물 등록일자", example = "2024-09-01")
  private String createdDate;

  public FacilityListResponse(FacilityRecord facility) {
    this.facilityId = facility.getFcltyId();
    this.type = facility.getFcltyTyp();
    this.name = facility.getFcltyNm();
    this.location = facility.getFcltyLctn();
    this.detailLocation = facility.getFcltyDtlLctn();
    this.information = facility.getFcltyInfo();
    this.department = facility.getFcltyDprNm();
    this.departmentPhoneNumber = facility.getFcltyDprTlphNmbr();
    this.approvalStatus = facility.getFcltyAprvSta();
    this.memberId = facility.getMbrId();
    this.createdDate = DateTimeUtils.convertToString(facility.getCreDtm(), DateTimeUtils.DEFAULT_DATE);
  }
}
