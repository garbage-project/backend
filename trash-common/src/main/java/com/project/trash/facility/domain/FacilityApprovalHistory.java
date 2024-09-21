package com.project.trash.facility.domain;

import com.project.trash.facility.domain.enums.FacilityApprovalStatus;

import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import java.time.LocalDateTime;

import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.EntityListeners;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.JoinColumn;
import jakarta.persistence.ManyToOne;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.NoArgsConstructor;

@Entity
@Getter
@NoArgsConstructor
@Table(name = "FACILITY_APPROVAL_HISTORY")
@EntityListeners(AuditingEntityListener.class)
public class FacilityApprovalHistory {

  @Id
  @Column(name = "FCLTY_APRV_HST_ID", nullable = false)
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long historyId;

  @Convert(converter = FacilityApprovalStatus.TypeCodeConverter.class)
  @Column(name = "FCLTY_APRV_STA", nullable = false)
  private FacilityApprovalStatus status;

  @CreatedDate
  @Column(name = "CRE_DTM", updatable = false, nullable = false)
  private LocalDateTime createdAt;

  @ManyToOne
  @JoinColumn(name = "FCLTY_ID")
  private Facility facility;

  public FacilityApprovalHistory(FacilityApprovalStatus status) {
    this.status = status;
  }

  void setFacility(Facility facility) {
    this.facility = facility;
  }
}
