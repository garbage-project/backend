package com.project.trash.member.domain;

import com.project.trash.member.domain.enums.SocialType;

import org.springframework.security.core.GrantedAuthority;
import org.springframework.security.core.authority.SimpleGrantedAuthority;
import org.springframework.security.core.userdetails.UserDetails;

import java.util.Collection;
import java.util.List;

import lombok.Getter;

@Getter
public class MemberDetail implements UserDetails {

  private final Long memberId;
  private final String nickname;
  private final String email;
  private final String socialId;
  private final SocialType socialType;

  public MemberDetail(Member member) {
    this.memberId = member.getMemberId();
    this.nickname = member.getNickname();
    this.email = member.getEmail();
    this.socialId = member.getSocialId();
    this.socialType = member.getSocialType();
  }

  @Override
  public Collection<? extends GrantedAuthority> getAuthorities() {
    return List.of(new SimpleGrantedAuthority("M"));
  }

  @Override
  public String getPassword() {
    return null;
  }

  @Override
  public String getUsername() {
    return this.socialId;
  }

  @Override
  public boolean isAccountNonExpired() {
    return true;
  }

  @Override
  public boolean isAccountNonLocked() {
    return true;
  }

  @Override
  public boolean isCredentialsNonExpired() {
    return true;
  }

  @Override
  public boolean isEnabled() {
    return true;
  }
}
