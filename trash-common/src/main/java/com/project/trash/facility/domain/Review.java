package com.project.trash.facility.domain;

import com.project.trash.common.domain.BaseTimeEntity;

import org.springframework.data.annotation.CreatedBy;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 리뷰 엔티티
 */
@Entity
@Getter
@NoArgsConstructor
@Table(name = "REVIEW")
public class Review extends BaseTimeEntity {

  /**
   * 리뷰 일련번호
   */
  @Id
  @Column(name = "RVW_SEQ", nullable = false)
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long reviewSeq;
  /**
   * 내용
   */
  @Column(name = "RVW_CTT", nullable = false)
  private String content;
  /**
   * 시설물 일련번호
   */
  @Column(name = "FCLTY_SEQ", nullable = false)
  private Long facilitySeq;
  /**
   * 회원 일련번호
   */
  @CreatedBy
  @Column(name = "MBR_SEQ", updatable = false, nullable = false)
  private Long memberSeq;

  public Review(String content, Long facilitySeq) {
    this.content = content;
    this.facilitySeq = facilitySeq;
  }

  public void update(String content) {
    this.content = content;
  }
}
