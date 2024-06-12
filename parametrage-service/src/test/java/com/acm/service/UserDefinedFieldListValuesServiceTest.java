/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doReturn;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URI;
import java.net.URL;
import java.util.Collections;
import java.util.Optional;

import org.assertj.core.api.Assertions;
import org.dozer.DozerBeanMapper;
import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.core.env.Environment;
import org.springframework.core.io.Resource;
import org.springframework.test.context.junit4.SpringRunner;

import com.acm.ApplicationProperties;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.UserDefinedFieldListValuesRepository;
import com.acm.service.impl.UserDefinedFieldListValuesServiceImpl;
import com.acm.utils.dtos.UserDefinedFieldListValuesDTO;
import com.acm.utils.models.UserDefinedFieldListValues;

/**
 * {@link UserDefinedFieldListValuesServiceTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
public class UserDefinedFieldListValuesServiceTest {

	/** The user defined field list values service. */
	@InjectMocks
	private UserDefinedFieldListValuesServiceImpl userDefinedFieldListValuesService;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The user defined field list values repository. */
	@Mock
	private UserDefinedFieldListValuesRepository userDefinedFieldListValuesRepository;

	/** The environment. */
	@Mock
	private Environment environment;

	/**
	 * Sets the up.
	 *
	 * @throws IOException Signals that an I/O exception has occurred.
	 */
	@BeforeEach
	void setUp() throws IOException {

		ApplicationProperties applicationProperties = new ApplicationProperties();

		Resource storage = new Resource() {

			@Override
			public InputStream getInputStream() throws IOException {

				return null;
			}

			@Override
			public long lastModified() throws IOException {

				return 0;
			}

			@Override
			public URL getURL() throws IOException {

				URL url = new URL("CustomURI:bl");
				return url;
			}

			@Override
			public URI getURI() throws IOException {

				return null;
			}

			@Override
			public String getFilename() {

				return null;
			}

			@Override
			public File getFile() throws IOException {

				return null;
			}

			@Override
			public String getDescription() {

				return null;
			}

			@Override
			public boolean exists() {

				return false;
			}

			@Override
			public Resource createRelative(String relativePath) throws IOException {

				return null;
			}

			@Override
			public long contentLength() throws IOException {

				return 0;
			}
		};

		applicationProperties.setStorageLocation(storage);

		userDefinedFieldListValuesService =
				new UserDefinedFieldListValuesServiceImpl(applicationProperties);
		MockitoAnnotations.initMocks(this);
		mapper.setMappingFiles(Collections.singletonList("dozer-configration-mapping.xml"));
	}

	/**
	 * Should success find by id.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Disabled
	@Test
	void shouldSuccessFindById() throws ResourcesNotFoundException {

		// GIVEN
		given(userDefinedFieldListValuesRepository.findById(any(Long.class)))
				.willReturn(Optional.of(new UserDefinedFieldListValues()));
		doReturn(new UserDefinedFieldListValuesDTO()).when(mapper)
				.map(new UserDefinedFieldListValues(), UserDefinedFieldListValuesDTO.class);

		// WHEN
		UserDefinedFieldListValuesDTO result = userDefinedFieldListValuesService.find(new Long(1));
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should fail find by.
	 * 
	 * @author ManelLamloum
	 */
	@Disabled
	@Test
	void shouldFailFindBy() {

		// GIVEN
		given(userDefinedFieldListValuesRepository.findById(any(Long.class)))
				.willReturn(Optional.empty());
		// WHEN
		try {
			userDefinedFieldListValuesService.find(new Long(1));
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("UserDefinedFieldListValues with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}
}
