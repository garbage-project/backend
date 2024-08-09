package com.project.trash.auth.domain;

import com.project.trash.member.domain.enums.GenderType;
import com.project.trash.member.domain.enums.SocialType;

public record OAuthMember(String socialId, String name, String email, GenderType gender, String birthday,
                          SocialType socialType) {
}
