package com.project.trash.report.response;

import com.project.trash.common.domain.enums.Valid;
import com.project.trash.common.utils.DateTimeUtils;
import com.project.trash.facility.domain.Facility;

import org.jooq.Record2;
import org.jooq.types.ULong;

import java.util.List;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Getter;
import lombok.Setter;
import trash.tables.records.ReportRecord;

/**
 * 신고 상세 조회 응답
 */
@Getter
@Setter
@Schema(title = "신고 상세 조회 응답")
public class ReportDetailResponse {

  @Schema(description = "신고 ID", example = "1")
  private final ULong reportId;

  @Schema(description = "신고 내용", example = "현재는 해당 시설물이 존재하지 않습니다.")
  private final String content;

  @Schema(description = "답변", example = "처리 완료했습니다!")
  private final String answer;

  @Schema(description = "신고 처리상태(Y - 처리완료, N - 미처리)", example = "Y")
  private final String status;

  @Schema(description = "신고 등록일자", example = "2024-09-01")
  private final String createdDate;

  @Schema(description = "회원 ID", example = "1")
  private final ULong memberId;

  @Schema(description = "회원 닉네임", example = "Kim")
  private final String nickname;
  /**
   * 시설물 정보
   */
  private final FacilityDetail facility;

  public ReportDetailResponse(Record2<ReportRecord, String> reportDetail, Facility facility, String s3ImageUrl) {
    ReportRecord report = reportDetail.value1();
    this.reportId = report.getRptId();
    this.content = report.getRptCtt();
    this.answer = report.getRptAns();
    this.status = report.getRptAns() != null ? Valid.TRUE.getCode() : Valid.FALSE.getCode();
    this.createdDate = DateTimeUtils.convertToString(report.getCreDtm(), DateTimeUtils.DEFAULT_DATE);
    this.memberId = report.getMbrId();
    this.nickname = reportDetail.value2();
    this.facility = new FacilityDetail(facility, s3ImageUrl);
  }

  @Getter
  @Setter
  public static class FacilityDetail {

    /**
     * 시설물 ID
     */
    @Schema(description = "시설물 ID", example = "1")
    private final Long facilityId;
    /**
     * 시설물 종류
     */
    @Schema(description = "시설물 종류 목록(R - 화장실, S - 흡연구역, T - 쓰레기통)", example = "R")
    private final String type;
    /**
     * 시설물명
     */
    @Schema(description = "시설물명", example = "쌍문역 내 화장실")
    private final String name;
    /**
     * 위치
     */
    @Schema(description = "위치", example = "쌍문역")
    private final String location;
    /**
     * 상세 위치
     */
    @Schema(description = "상세 위치", example = "지하 1층")
    private final String detailLocation;
    /**
     * 정보
     */
    @Schema(description = "정보", example = "개찰구 내에 존재합니다.", nullable = true)
    private final String information;
    /**
     * 관리 부서
     */
    @Schema(description = "관리 부서", example = "서울시설공단")
    private final String department;
    /**
     * 관리 부서 전화번호
     */
    @Schema(description = "관리 부서 전화번호", example = "02-2290-7111")
    private final String departmentPhoneNumber;
    /**
     * 승인 상태
     */
    @Schema(description = "승인 상태(P - 승인대기, A - 승인완료, R - 승인거절, S - 승인중단)", example = "A")
    private final String approvalStatus;
    /**
     * 등록일자
     */
    @Schema(description = "시설물 등록일자", example = "2024-09-01")
    private final String createdDate;
    /**
     * 이미지 목록
     */
    private List<String> images;

    public FacilityDetail(Facility facility, String s3ImageUrl) {
      this.facilityId = facility.getFacilityId();
      this.name = facility.getName();
      this.type = facility.getType().getCode();
      this.location = facility.getLocation();
      this.detailLocation = facility.getDetailLocation();
      this.information = facility.getInformation();
      this.department = facility.getDepartment();
      this.departmentPhoneNumber = facility.getDepartmentPhoneNumber();
      this.approvalStatus = facility.getApprovalStatus().getCode();
      this.createdDate = DateTimeUtils.convertToString(facility.getCreatedAt(), DateTimeUtils.DEFAULT_DATE);
    }
  }
}
