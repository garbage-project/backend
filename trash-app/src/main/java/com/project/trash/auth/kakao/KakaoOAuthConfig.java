package com.project.trash.auth.kakao;

import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Kakao OAuth 설정 정보
 */
@ConfigurationProperties(prefix = "oauth2.kakao")
public record KakaoOAuthConfig(

    String redirectUri, String clientId, String clientSecret, String[] scope, String authorizeUri, String tokenUri,
    String tokenInfoUri, String userInfoUri, String authorizationPrefix) {

}
