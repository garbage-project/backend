FROM openjdk:21

ENV TZ=Asia/Seoul

# copy jar
COPY build/libs/*.jar admin-server.jar

ENTRYPOINT ["java", "-Dspring.profiles.active=dev", "-jar", "admin-server.jar"]