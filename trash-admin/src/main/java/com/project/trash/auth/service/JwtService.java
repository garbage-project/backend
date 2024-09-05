package com.project.trash.auth.service;

import com.project.trash.admin.domain.Admin;
import com.project.trash.auth.config.JwtConfig;

import org.apache.commons.lang3.tuple.Pair;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.stereotype.Service;

import java.security.Key;
import java.util.Date;
import java.util.function.Function;

import io.jsonwebtoken.Claims;
import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.io.Decoders;
import io.jsonwebtoken.security.Keys;
import lombok.RequiredArgsConstructor;

/**
 * JWT 서비스
 */
@Service
@RequiredArgsConstructor
public class JwtService {

  private final JwtConfig jwtConfig;

  /**
   * 엑세스 토큰 발급
   */
  public Pair<String, Integer> createAccessToken(String socialId) {
    String token = Jwts.builder()
                       .setSubject(socialId)
                       .setIssuedAt(new Date(System.currentTimeMillis()))
                       .setExpiration(new Date(System.currentTimeMillis() + jwtConfig.accessExpiration()))
                       .signWith(getSignInKey(), SignatureAlgorithm.HS512)
                       .compact();

    return Pair.of(token, jwtConfig.accessExpiration());
  }

  /**
   * 리프레시 토큰 발급
   */
  public Pair<String, Integer> createRefreshToken(String socialId) {
    String token = Jwts.builder()
                       .setSubject(socialId)
                       .setIssuedAt(new Date(System.currentTimeMillis()))
                       .setExpiration(new Date(System.currentTimeMillis() + jwtConfig.refreshExpiration()))
                       .signWith(getSignInKey(), SignatureAlgorithm.HS512)
                       .compact();

    return Pair.of(token, jwtConfig.refreshExpiration());
  }

  /**
   * 토큰에서 고유값 추출
   */
  public String extractUsername(String token) {
    return extractClaim(token, Claims::getSubject);
  }

  /**
   * 토큰 유효성 검사
   */
  public boolean isTokenValid(String token, UserDetails userDetails) {
    final String username = extractUsername(token);
    return (username.equals(userDetails.getUsername())) && !isTokenExpired(token);
  }

  public boolean isTokenValid(String token, Admin admin) {
    final String username = extractUsername(token);
    return (username.equals(admin.getId())) && !isTokenExpired(token);
  }

  private Claims extractAllClaims(String token) {
    return Jwts.parserBuilder().setSigningKey(getSignInKey()).build().parseClaimsJws(token).getBody();
  }

  private <T> T extractClaim(String token, Function<Claims, T> claimsResolver) {
    final Claims claims = extractAllClaims(token);
    return claimsResolver.apply(claims);
  }

  private Date extractExpiration(String token) {
    return extractClaim(token, Claims::getExpiration);
  }

  private Key getSignInKey() {
    byte[] keyBytes = Decoders.BASE64.decode(jwtConfig.secretKey());
    return Keys.hmacShaKeyFor(keyBytes);
  }

  private boolean isTokenExpired(String token) {
    return extractExpiration(token).before(new Date());
  }
}
