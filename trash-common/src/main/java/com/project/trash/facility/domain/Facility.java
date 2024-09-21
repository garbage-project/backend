package com.project.trash.facility.domain;

import com.project.trash.common.domain.BaseTimeEntity;
import com.project.trash.facility.domain.enums.FacilityApprovalStatus;
import com.project.trash.facility.domain.enums.FacilityType;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import jakarta.persistence.CascadeType;
import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.FetchType;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.OneToMany;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 시설물 엔티티
 */
@Entity
@Getter
@NoArgsConstructor
@Table(name = "FACILITY")
public class Facility extends BaseTimeEntity {

  @Id
  @Column(name = "FCLTY_ID", nullable = false)
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long facilityId;

  @Column(name = "FCLTY_NM", nullable = false)
  private String name;

  @Convert(converter = FacilityType.TypeCodeConverter.class)
  @Column(name = "FCLTY_TYP", nullable = false)
  private FacilityType type;

  @Column(name = "FCLTY_LCTN", nullable = false)
  private String location;

  @Column(name = "FCLTY_DTL_LCTN")
  private String detailLocation;

  @Column(name = "FCLTY_LTTD", nullable = false)
  private BigDecimal latitude;

  @Column(name = "FCLTY_LNGT", nullable = false)
  private BigDecimal longitude;

  @Column(name = "FCLTY_INFO")
  private String information;

  @Column(name = "FCLTY_DPR_NM")
  private String department;

  @Column(name = "FCLTY_DPR_TLPH_NMBR")
  private String departmentPhoneNumber;

  @Convert(converter = FacilityApprovalStatus.TypeCodeConverter.class)
  @Column(name = "FCLTY_APRV_STA", nullable = false)
  private FacilityApprovalStatus approvalStatus = FacilityApprovalStatus.PENDING;

  @Column(name = "MBR_ID", updatable = false)
  private String memberId;

  @OneToMany(mappedBy = "facility", fetch = FetchType.LAZY, orphanRemoval = true,
      cascade = {CascadeType.PERSIST, CascadeType.MERGE})
  private List<FacilityImage> images = new ArrayList<>();

  @OneToMany(mappedBy = "facility", fetch = FetchType.LAZY, orphanRemoval = true,
      cascade = {CascadeType.PERSIST, CascadeType.MERGE})
  private List<FacilityApprovalHistory> approvalHistories = new ArrayList<>();

  public Facility(FacilityType type, String name, String location, String detailLocation, BigDecimal latitude,
      BigDecimal longitude, String information, String memberId) {
    this.type = type;
    this.name = name;
    this.location = location;
    this.detailLocation = detailLocation;
    this.latitude = latitude;
    this.longitude = longitude;
    this.information = information;
    this.memberId = memberId;
  }

  public Facility(FacilityType type, String name, String location, String detailLocation, BigDecimal latitude,
      BigDecimal longitude, String information, String department, String departmentPhoneNumber,
      FacilityApprovalStatus approvalStatus, String memberId) {
    this.type = type;
    this.name = name;
    this.location = location;
    this.detailLocation = detailLocation;
    this.latitude = latitude;
    this.longitude = longitude;
    this.information = information;
    this.department = department;
    this.departmentPhoneNumber = departmentPhoneNumber;
    this.approvalStatus = approvalStatus;
    this.memberId = memberId;
  }

  // 엑셀 파일 업로드
  public Facility(String name, String location, String detailLocation, BigDecimal latitude,
      BigDecimal longitude, String department, String departmentPhoneNumber, FacilityType type) {
    this.name = name;
    this.location = location;
    this.detailLocation = detailLocation;
    this.latitude = latitude;
    this.longitude = longitude;
    this.department = department;
    this.departmentPhoneNumber = departmentPhoneNumber;
    this.type = type;
    this.approvalStatus = FacilityApprovalStatus.APPROVE;
  }

  public void update(FacilityType type, String name, String location, String detailLocation, BigDecimal latitude,
      BigDecimal longitude, String information) {
    this.type = type;
    this.name = name;
    this.location = location;
    this.detailLocation = detailLocation;
    this.latitude = latitude;
    this.longitude = longitude;
    this.information = information;

    // 시설물 수정 시, 승인 대기 상태로 변경
    if (approvalStatus != FacilityApprovalStatus.PENDING) {
      this.approvalStatus = FacilityApprovalStatus.PENDING;
      addApprovalHistory(new FacilityApprovalHistory(FacilityApprovalStatus.PENDING));
    }
  }

  public void update(FacilityType type, String name, String location, String detailLocation, BigDecimal latitude,
      BigDecimal longitude, String information, String department, String departmentPhoneNumber,
      FacilityApprovalStatus approvalStatus) {
    this.type = type;
    this.name = name;
    this.location = location;
    this.detailLocation = detailLocation;
    this.latitude = latitude;
    this.longitude = longitude;
    this.information = information;
    this.department = department;
    this.departmentPhoneNumber = departmentPhoneNumber;

    if (this.approvalStatus != approvalStatus) {
      addApprovalHistory(new FacilityApprovalHistory(approvalStatus));
    }
    this.approvalStatus = approvalStatus;
  }

  private void addApprovalHistory(FacilityApprovalHistory approvalHistory) {
    this.approvalHistories.add(approvalHistory);
    approvalHistory.setFacility(this);
  }
}
