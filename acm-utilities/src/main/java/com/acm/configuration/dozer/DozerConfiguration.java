/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.configuration.dozer;

import java.util.Arrays;
import java.util.List;

import org.dozer.DozerBeanMapper;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * {@link DozerConfiguration} for Dozer related beans.
 * 
 * @author HaythemBenizid
 * @since 0.1.0
 */
@Configuration
public class DozerConfiguration {

	/**
	 * Dozer bean.
	 *
	 * @return the dozer bean mapper
	 */
	@Bean(name = "org.dozer.Mapper")
	public DozerBeanMapper dozerBean() {

		List<String> mappingFiles = Arrays.asList("dozer-configration-mapping.xml");

		DozerBeanMapper dozerBean = new DozerBeanMapper();
		dozerBean.setMappingFiles(mappingFiles);
		return dozerBean;
	}
}
