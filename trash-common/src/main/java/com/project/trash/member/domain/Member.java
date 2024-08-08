package com.project.trash.member.domain;

import com.project.trash.common.domain.BaseTimeEntity;
import com.project.trash.member.domain.enums.GenderType;
import com.project.trash.member.domain.enums.SocialType;

import jakarta.persistence.Column;
import jakarta.persistence.Convert;
import jakarta.persistence.Entity;
import jakarta.persistence.GeneratedValue;
import jakarta.persistence.GenerationType;
import jakarta.persistence.Id;
import jakarta.persistence.Table;
import lombok.Getter;
import lombok.NoArgsConstructor;

/**
 * 회원 엔티티
 */
@Entity
@Getter
@NoArgsConstructor
@Table(name = "MEMBER")
public class Member extends BaseTimeEntity {

  /**
   * 회원 일련번호
   */
  @Id
  @Column(name = "MBR_SEQ", nullable = false)
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long memberSeq;
  /**
   * 이메일
   */
  @Column(name = "MBR_EMAIL", nullable = false)
  private String email;
  /**
   * 이름
   */
  @Column(name = "MBR_NM", nullable = false)
  private String name;
  /**
   * 성별
   */
  @Convert(converter = GenderType.TypeCodeConverter.class)
  @Column(name = "MBR_GNDR", nullable = false)
  private GenderType gender;
  /**
   * 생년월일
   */
  @Column(name = "MBR_BRDT", nullable = false)
  private String birthday;
  /**
   * 소셜 ID
   */
  @Column(name = "MBR_SCL_ID", nullable = false)
  private String socialId;
  /**
   * 소셜 타입
   */
  @Convert(converter = SocialType.TypeCodeConverter.class)
  @Column(name = "MBR_SCL_TYP", nullable = false)
  private String socialType;
  /**
   * 유효여부
   */
  @Column(name = "MBR_VALID_YN", nullable = false)
  private Boolean valid = Boolean.TRUE;

  public Member(String email, String name, GenderType gender, String birthday, String socialId, String socialType) {
    this.email = email;
    this.name = name;
    this.gender = gender;
    this.birthday = birthday;
    this.socialId = socialId;
    this.socialType = socialType;
  }
}
