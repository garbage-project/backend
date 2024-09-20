package com.project.trash.config;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.List;

import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.servers.Server;

@Configuration
public class SwaggerConfig {

  @Value("${springdoc.swagger-ui.server-url}")
  private String serverUrl;

  @Bean
  public OpenAPI openAPI() {
    Server server = new Server();
    server.setUrl(serverUrl);

    return new OpenAPI()
        .components(new Components())
        .info(apiInfo())
        .servers(List.of(server));
  }

  private Info apiInfo() {
    return new Info()
        .title("Spotfinder Admin API Document")
        .description("Spotfinder Admin API Document")
        .version("1.0.0");
  }
}
