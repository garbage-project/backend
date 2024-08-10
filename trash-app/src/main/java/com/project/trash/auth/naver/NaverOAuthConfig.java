package com.project.trash.auth.naver;

import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * Naver OAuth 설정 정보
 */
@ConfigurationProperties(prefix = "oauth2.naver")
public record NaverOAuthConfig(

    String redirectUri, String clientId, String clientSecret, String[] scope, String authorizeUri, String tokenUri,
    String userInfoUri, String authorizationPrefix) {

}
