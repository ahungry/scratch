FROM openjdk:8-alpine

COPY target/uberjar/micro-graphql.jar /micro-graphql/app.jar

EXPOSE 3000

CMD ["java", "-jar", "/micro-graphql/app.jar"]
