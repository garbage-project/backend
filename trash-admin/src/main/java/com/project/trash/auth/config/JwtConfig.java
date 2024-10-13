package com.project.trash.auth.config;

import org.springframework.boot.context.properties.ConfigurationProperties;

@ConfigurationProperties(prefix = "jwt")
public record JwtConfig(String secretKey, Long accessExpiration, Long refreshExpiration) {
}
