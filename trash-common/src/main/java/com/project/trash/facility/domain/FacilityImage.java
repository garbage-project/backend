package com.project.trash.facility.domain;

import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import java.time.LocalDateTime;

import jakarta.persistence.Column;
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
@Table(name = "FACILITY_IMAGE")
@EntityListeners(AuditingEntityListener.class)
public class FacilityImage {

  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  @Column(name = "FCLTY_IMG_ID")
  private Long imgId;

  @Column(name = "FCLTY_IMG_PATH", nullable = false)
  private String path;

  @ManyToOne
  @JoinColumn(name = "FCLTY_ID")
  private Facility facility;

  @CreatedDate
  @Column(name = "CRE_DTM", updatable = false, nullable = false)
  private LocalDateTime createdAt;

  public FacilityImage(String path) {
    this.path = path;
  }

  public void setFacility(Facility facility) {
    this.facility = facility;
  }
}
