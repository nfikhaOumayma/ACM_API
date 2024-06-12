/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.configuration.swagger;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.Collections;

import org.apache.maven.model.Model;
import org.apache.maven.model.io.xpp3.MavenXpp3Reader;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import com.netflix.discovery.shared.Application;

import springfox.documentation.builders.PathSelectors;
import springfox.documentation.builders.RequestHandlerSelectors;
import springfox.documentation.service.ApiInfo;
import springfox.documentation.service.Contact;
import springfox.documentation.spi.DocumentationType;
import springfox.documentation.spring.web.plugins.Docket;
import springfox.documentation.swagger2.annotations.EnableSwagger2;

/**
 * {@link SwaggerConfig} class.
 *
 * @author YesserSomai
 * @since 1.1.3
 */
@Configuration
@EnableSwagger2
public class SwaggerConfig {

	/**
	 * Api.
	 *
	 * @return the docket
	 * @throws FileNotFoundException the file not found exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws XmlPullParserException the xml pull parser exception
	 */
	@Bean
	public Docket api() throws IOException, XmlPullParserException {

		return new Docket(DocumentationType.SWAGGER_2).select()
				.apis(RequestHandlerSelectors.basePackage("com.acm.controller"))
				.paths(PathSelectors.any()).build().apiInfo(apiInfo());
	}

	/**
	 * Api info.
	 *
	 * @return the api info
	 * @throws FileNotFoundException the file not found exception
	 * @throws IOException Signals that an I/O exception has occurred.
	 * @throws XmlPullParserException the xml pull parser exception
	 */
	private ApiInfo apiInfo() throws IOException, XmlPullParserException {

		MavenXpp3Reader mavenXpp3Reader = new MavenXpp3Reader();
		Model model;
		if ((new File("pom.xml")).exists()) {
			model = mavenXpp3Reader.read(new FileReader("pom.xml"));
		}
		else {
			// Packaged artifacts contain a META-INF/maven/${groupId}/${artifactId}/pom.properties
			model = mavenXpp3Reader.read(new InputStreamReader(Application.class
					.getResourceAsStream("/META-INF/maven/com.acm/expenses-service/pom.xml")));
		}
		return new ApiInfo("Advanced Credit Management (ACM)", model.getDescription(),
				model.getParent().getVersion(), "Terms of TALYS",
				new Contact("ACM", "www.talys-consulting.com", "info@talys-consulting.com"),
				"License of API", "www.talys-consulting.com", Collections.emptyList());
	}
}
