package com.project.trash.auth.config;

import org.springframework.boot.context.properties.ConfigurationProperties;

/**
 * JWT 설정 정보
 */
@ConfigurationProperties(prefix = "jwt")
public record JwtConfig(String secretKey, Integer accessExpiration, Integer refreshExpiration) {
}
