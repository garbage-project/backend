package com.project.trash.notice.domain;

import com.project.trash.common.domain.BaseTimeEntity;

import jakarta.persistence.Column;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 공지 엔티티
 */
@Entity
@Getter
@NoArgsConstructor
@Table(name = "NOTICE")
public class Notice extends BaseTimeEntity {

  /**
   * 공지 ID
   */
  @Id
  @Column(name = "NTC_ID", nullable = false)
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long noticeId;
  /**
   * 공지 제목
   */
  @Column(name = "NTC_TTL", nullable = false)
  private String title;
  /**
   * 공지 내용
   */
  @Column(name = "NTC_CTT", nullable = false)
  private String content;
  /**
   * 유효여부
   */
  @Column(name = "NTC_VLD_YN", nullable = false)
  private Boolean valid;

  public Notice(String title, String content, Boolean valid) {
    this.title = title;
    this.content = content;
    this.valid = valid;
  }

  public void update(String title, String content, Boolean valid) {
    this.title = title;
    this.content = content;
    this.valid = valid;
  }
}
