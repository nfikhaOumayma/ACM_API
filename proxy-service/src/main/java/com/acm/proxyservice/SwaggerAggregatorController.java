/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.proxyservice;

import java.util.ArrayList;
import java.util.List;

import org.springframework.boot.autoconfigure.EnableAutoConfiguration;
import org.springframework.context.annotation.Primary;
import org.springframework.stereotype.Component;

import springfox.documentation.swagger.web.SwaggerResource;
import springfox.documentation.swagger.web.SwaggerResourcesProvider;

/**
 * {@link SwaggerAggregatorController} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Component
@Primary
@EnableAutoConfiguration
public class SwaggerAggregatorController implements SwaggerResourcesProvider {

	/*
	 * (non-Javadoc)
	 * @see com.google.common.base.Supplier#get()
	 */
	@Override
	public List<SwaggerResource> get() {

		List<SwaggerResource> resources = new ArrayList<>();
		SwaggerResource swaggerResource;

		/*
		 * authentication-service
		 */
		swaggerResource = createSwaggerResource("authentication-service",
				"/authentication-service/v2/api-docs");
		resources.add(swaggerResource);

		/*
		 * credit-service
		 */
		swaggerResource = createSwaggerResource("credit-service", "/credit-service/v2/api-docs");
		resources.add(swaggerResource);

		/*
		 * crm-service
		 */
		swaggerResource = createSwaggerResource("crm-service", "/crm-service/v2/api-docs");
		resources.add(swaggerResource);

		/*
		 * expenses-service
		 */
		swaggerResource =
				createSwaggerResource("expenses-service", "/expenses-service/v2/api-docs");
		resources.add(swaggerResource);

		/*
		 * ged-service
		 */
		swaggerResource = createSwaggerResource("ged-service", "/ged-service/v2/api-docs");
		resources.add(swaggerResource);

		/*
		 * incentive-service
		 */
		swaggerResource =
				createSwaggerResource("incentive-service", "/incentive-service/v2/api-docs");
		resources.add(swaggerResource);

		/*
		 * parametrage-service
		 */
		swaggerResource =
				createSwaggerResource("parametrage-service", "/parametrage-service/v2/api-docs");
		resources.add(swaggerResource);

		/*
		 * reporting-service
		 */
		swaggerResource =
				createSwaggerResource("reporting-service", "/reporting-service/v2/api-docs");
		resources.add(swaggerResource);

		/*
		 * transvers-service
		 */
		swaggerResource =
				createSwaggerResource("transvers-service", "/transvers-service/v2/api-docs");
		resources.add(swaggerResource);
		
		/*
		 * dashboarding-service
		 */
		swaggerResource =
				createSwaggerResource("dashboarding-service", "/dashboarding-service/v2/api-docs");
		resources.add(swaggerResource);

		return resources;
	}

	/**
	 * Create a new swagger ressource with name, location and version.
	 *
	 * @param name the resource name
	 * @param location the resource location
	 * @return the created resource
	 */
	private SwaggerResource createSwaggerResource(String name, String location) {

		SwaggerResource swaggerResource = new SwaggerResource();
		swaggerResource.setName(name);
		swaggerResource.setLocation(location);
		swaggerResource.setSwaggerVersion("2.0");
		return swaggerResource;
	}
}
