package com.project.trash.report.response;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.project.trash.common.domain.enums.Valid;
import com.project.trash.common.utils.DateTimeUtils;
import com.project.trash.facility.domain.Facility;

import org.jooq.Record2;
import org.jooq.types.ULong;

import java.util.List;

import lombok.Getter;
import lombok.Setter;
import trash.tables.records.ReportRecord;

/**
 * 신고 상세 조회 응답
 */
@Getter
@Setter
public class ReportDetailResponse {

  /**
   * 신고 일련번호
   */
  private final ULong reportSeq;
  /**
   * 내용
   */
  private final String content;
  /**
   * 답변
   */
  private final String answer;
  /**
   * 상태
   */
  private final String status;
  /**
   * 등록일자
   */
  private final String createdDate;
  /**
   * 회원 일련번호(신고자 ID)
   */
  private final ULong memberSeq;
  /**
   * 회원 닉네임
   */
  private final String nickname;
  /**
   * 시설물 상세 정보
   */
  private final FacilityDetail facility;

  public ReportDetailResponse(Record2<ReportRecord, String> reportDetail, Facility facility, String s3ImageUrl) {
    ReportRecord report = reportDetail.value1();
    this.reportSeq = report.getRptSeq();
    this.content = report.getRptCtt();
    this.answer = report.getRptAns();
    this.status = report.getRptAns() != null ? Valid.TRUE.getCode() : Valid.FALSE.getCode();
    this.createdDate = DateTimeUtils.convertToString(report.getCreDtm(), DateTimeUtils.DEFAULT_DATE);
    this.memberSeq = report.getMbrSeq();
    this.nickname = reportDetail.value2();
    this.facility = new FacilityDetail(facility, s3ImageUrl);
  }

  @Getter
  @Setter
  @JsonInclude(JsonInclude.Include.NON_NULL)
  public static class FacilityDetail {

    /**
     * 시설물 ID
     */
    private final String facilityId;
    /**
     * 시설물명
     */
    private final String name;
    /**
     * 시설물 종류
     */
    private final String type;
    /**
     * 위치
     */
    private final String location;
    /**
     * 상세 위치
     */
    private final String detailLocation;
    /**
     * 설명
     */
    private final String information;
    /**
     * 관리 부서
     */
    private final String department;
    /**
     * 관리 부서 전화번호
     */
    private final String departmentPhoneNumber;
    /**
     * 승인 상태
     */
    private final String approvalStatus;
    /**
     * 등록일자
     */
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

      if (facility.getImages() != null) {
        this.images = facility.getImages().stream().map(image -> s3ImageUrl + image).toList();
      }
    }
  }
}
