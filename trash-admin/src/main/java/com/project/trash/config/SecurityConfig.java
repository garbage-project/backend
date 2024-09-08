package com.project.trash.config;

import com.project.trash.admin.service.AdminQueryService;
import com.project.trash.auth.filter.JwtAuthenticationFilter;
import com.project.trash.auth.service.JwtService;
import com.project.trash.common.exception.handler.CustomAccessDeniedHandler;
import com.project.trash.common.exception.handler.CustomAuthenticationEntryPoint;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.config.http.SessionCreationPolicy;
import org.springframework.security.web.SecurityFilterChain;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;

import lombok.RequiredArgsConstructor;

@Configuration
@EnableWebSecurity
@RequiredArgsConstructor
public class SecurityConfig {

  private final JwtService jwtService;
  private final AdminQueryService adminQueryService;

  private final CustomAuthenticationEntryPoint customAuthenticationEntryPoint;
  private final CustomAccessDeniedHandler customAccessDeniedHandler;

  @Bean
  public SecurityFilterChain securityFilterChain(HttpSecurity http) throws Exception {
    http
        // CSRF 보안 비활성화
        .csrf(AbstractHttpConfigurer::disable)
        .formLogin(AbstractHttpConfigurer::disable)
        .httpBasic(AbstractHttpConfigurer::disable)
        // 세션을 생성하지 않게 설정
        .sessionManagement(session -> session.sessionCreationPolicy(SessionCreationPolicy.STATELESS))
        .authorizeHttpRequests(
            (authorize) -> authorize.requestMatchers("/swagger-ui/**", "/v3/api-docs/**", "/doc", "/health",
                "/admins/login", "/admins/reissue").permitAll().anyRequest().authenticated())
        .addFilterBefore(new JwtAuthenticationFilter(jwtService, adminQueryService),
            UsernamePasswordAuthenticationFilter.class)
        .exceptionHandling(it -> {
          it.authenticationEntryPoint(customAuthenticationEntryPoint);
          it.accessDeniedHandler(customAccessDeniedHandler);
        })
        .securityContext((securityContext) -> securityContext.requireExplicitSave(false));

    return http.build();
  }
}
