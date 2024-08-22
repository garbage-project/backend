FROM openjdk:21

ENV TZ=Asia/Seoul

# copy jar
COPY build/libs/*.jar app-server.jar

ENTRYPOINT ["java", "-Dspring.profiles.active=dev", "-jar", "app-server.jar"]