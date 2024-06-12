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
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.assertj.core.api.Assertions;
import org.dozer.DozerBeanMapper;
import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.mockito.Spy;
import org.springframework.core.env.Environment;
import org.springframework.test.context.junit4.SpringRunner;

import com.acm.client.TransversClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonConstants;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.SettingListValuesRepository;
import com.acm.service.impl.SettingListValuesServiceImpl;
import com.acm.utils.dtos.BrancheDTO;
import com.acm.utils.dtos.IndustryDTO;
import com.acm.utils.dtos.LoanGuarantorSourceDTO;
import com.acm.utils.dtos.LoanRefinanceReasonDTO;
import com.acm.utils.dtos.LoanSourceOfFundsDTO;
import com.acm.utils.dtos.PortfolioDTO;
import com.acm.utils.dtos.ProductLoanReasonsDTO;
import com.acm.utils.dtos.RelationshipDTO;
import com.acm.utils.dtos.RoleAbacusDTO;
import com.acm.utils.dtos.SettingListValuesDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.models.SettingListValues;

/**
 * {@link SettingListValuesServiceTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
public class SettingListValuesServiceTest {

	/** The setting list values service. */
	@InjectMocks
	private SettingListValuesServiceImpl settingListValuesService;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The user client. */
	@Mock
	private UserClient userClient;

	/** The setting list values repository. */
	@Mock
	private SettingListValuesRepository settingListValuesRepository;

	/** The environment. */
	@Mock
	private Environment environment;

	/** The setting transvers client. */
	@Mock
	private TransversClient settingTransversClient;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		mapper.setMappingFiles(Collections.singletonList("dozer-configration-mapping.xml"));
	}

	/**
	 * Inits the setting list values DTO.
	 *
	 * @return the setting list values DTO
	 */
	private SettingListValuesDTO initSettingListValuesDTO() {

		SettingListValuesDTO settingListValuesDTO = new SettingListValuesDTO();
		settingListValuesDTO.setId(new Long(1));
		settingListValuesDTO.setTableAbacusName("test");
		return settingListValuesDTO;
	}

	/**
	 * Inits the setting list values.
	 *
	 * @return the setting list values
	 */
	private SettingListValues initSettingListValues() {

		SettingListValues settingListValues = new SettingListValues();
		settingListValues.setId(new Long(1));
		settingListValues.setTableAbacusName("test");
		return settingListValues;
	}

	/**
	 * Inits the user DTO.
	 *
	 * @return the user DTO
	 */
	private UserDTO initUserDTO() {

		UserDTO userDTO = new UserDTO();
		userDTO.setNom("test");
		userDTO.setEmail("test@test.gmail");
		userDTO.setLogin(CommonConstants.DEFAULT_USER);
		userDTO.setPrenom("benTest");
		userDTO.setFullName("Test Ben Test Test");
		return userDTO;
	}

	/**
	 * Should D success find by id.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldDSuccessFindById() throws ResourcesNotFoundException {

		// GIVEN
		SettingListValuesDTO settingListValuesDTO = initSettingListValuesDTO();
		SettingListValues settingListValues = initSettingListValues();
		given(settingListValuesRepository.findById(any(Long.class)))
				.willReturn(Optional.of(settingListValues));
		doReturn(settingListValuesDTO).when(mapper).map(settingListValues,
				SettingListValuesDTO.class);

		// WHEN
		SettingListValuesDTO result = settingListValuesService.find(new Long(1));

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should return list setting list values DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldReturnListSettingListValuesDTO() {

		// GIVEN
		SettingListValues settingListValues = initSettingListValues();
		SettingListValuesDTO settingListValuesDTO = initSettingListValuesDTO();
		given(settingListValuesRepository.findAll())
				.willReturn(Collections.singletonList(settingListValues));
		doReturn(settingListValuesDTO).when(mapper).map(settingListValues,
				SettingListValuesDTO.class);

		// WHEN
		List<SettingListValuesDTO> result = settingListValuesService.find(settingListValuesDTO);
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should create setting list values DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldCreateSettingListValuesDTO() {

		// GIVEN
		SettingListValues settingListValues = initSettingListValues();
		SettingListValuesDTO settingListValuesDTO = initSettingListValuesDTO();

		doReturn(settingListValues).when(mapper).map(settingListValuesDTO, SettingListValues.class);
		doReturn(settingListValuesDTO).when(mapper).map(settingListValues,
				SettingListValuesDTO.class);
		given(settingListValuesRepository.save(any(SettingListValues.class)))
				.willReturn(settingListValues);
		doReturn(initUserDTO()).when(userClient).find();

		// WHEN
		SettingListValuesDTO result = settingListValuesService.save(settingListValuesDTO);
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success update list values DTO.
	 *
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessUpdateListValuesDTO() throws ResourcesNotFoundException {

		// GIVEN
		SettingListValues settingListValues = initSettingListValues();
		SettingListValuesDTO settingListValuesDTO = initSettingListValuesDTO();
		given(settingListValuesRepository.findById(any(Long.class)))
				.willReturn(Optional.of(settingListValues));
		given(settingListValuesRepository.save(any(SettingListValues.class)))
				.willReturn(settingListValues);

		doReturn(initUserDTO()).when(userClient).find();
		doReturn(settingListValuesDTO).when(mapper).map(settingListValues,
				SettingListValuesDTO.class);

		// WHEN
		SettingListValuesDTO result =
				settingListValuesService.save(settingListValuesDTO.getId(), settingListValuesDTO);
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should fail update list values DTO.
	 * 
	 * @author ManelLamloum
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldFailUpdateListValuesDTO() throws ResourcesNotFoundException {

		// GIVEN
		SettingListValuesDTO settingListValuesDTO = initSettingListValuesDTO();
		given(settingListValuesRepository.findById(any(Long.class))).willReturn(Optional.empty());
		// WHEN
		try {
			settingListValuesService.save(settingListValuesDTO.getId(), settingListValuesDTO);
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("SettingListValues with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should success find all portfolio.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessFindAllPortfolio() {

		// GIVEN
		given(settingTransversClient.findAllPortfolio())
				.willReturn(Collections.singletonList(new PortfolioDTO()));
		// WHEN
		List<PortfolioDTO> result = settingListValuesService.findAllPortfolio();
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success load setting from abacus head office case.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessLoadSettingFromAbacusNotHeadOfficeCase() {

		// GIVEN
		SettingListValues settingListValues = initSettingListValues();
		SettingListValuesDTO settingListValuesDTO = initSettingListValuesDTO();
		BrancheDTO brancheDTO = new BrancheDTO();
		brancheDTO.setName("Test");
		given(settingTransversClient.findBranches())
				.willReturn(Collections.singletonList(brancheDTO));
		given(settingTransversClient.findProductLoanReasons())
				.willReturn(Collections.singletonList(new ProductLoanReasonsDTO()));
		given(settingTransversClient.findLoanGuarantorSource())
				.willReturn(Collections.singletonList(new LoanGuarantorSourceDTO()));

		given(settingTransversClient.findLoanSourceOfFunds())
				.willReturn(Collections.singletonList(new LoanSourceOfFundsDTO()));

		given(settingTransversClient.findLoanRefinanceReason())
				.willReturn(Collections.singletonList(new LoanRefinanceReasonDTO()));

		given(settingTransversClient.findRelationship())
				.willReturn(Collections.singletonList(new RelationshipDTO()));

		given(settingTransversClient.findIndustry())
				.willReturn(Collections.singletonList(new IndustryDTO()));

		given(settingTransversClient.findRoleAbacus())
				.willReturn(Collections.singletonList(new RoleAbacusDTO()));

		doReturn(settingListValuesDTO).when(mapper).map(settingListValues,
				SettingListValuesDTO.class);
		doReturn(settingListValues).when(mapper).map(settingListValuesDTO, SettingListValues.class);
		doReturn(initUserDTO()).when(userClient).find();
		given(settingListValuesRepository.save(any(SettingListValues.class)))
				.willReturn(settingListValues);

		// WHEN
		settingListValuesService.loadSettingFromAbacus();
		// THEN
		verify(settingTransversClient, times(1)).findBranches();
		verify(settingTransversClient, times(1)).findProductLoanReasons();
		verify(settingTransversClient, times(1)).findLoanGuarantorSource();
		verify(settingTransversClient, times(1)).findLoanSourceOfFunds();
		verify(settingTransversClient, times(1)).findLoanRefinanceReason();
		verify(settingTransversClient, times(1)).findRelationship();
		verify(settingTransversClient, times(1)).findIndustry();
		verify(settingTransversClient, times(1)).findRoleAbacus();
	}

	/**
	 * Should success load setting from abacus head office case.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessLoadSettingFromAbacusHeadOfficeCase() {

		// GIVEN
		SettingListValues settingListValues = initSettingListValues();
		SettingListValuesDTO settingListValuesDTO = initSettingListValuesDTO();
		BrancheDTO brancheDTO = new BrancheDTO();
		brancheDTO.setName(CommonConstants.HEAD_OFFICE);
		given(settingTransversClient.findBranches())
				.willReturn(Collections.singletonList(brancheDTO));
		given(settingTransversClient.findProductLoanReasons())
				.willReturn(Collections.singletonList(new ProductLoanReasonsDTO()));
		given(settingTransversClient.findLoanGuarantorSource())
				.willReturn(Collections.singletonList(new LoanGuarantorSourceDTO()));

		given(settingTransversClient.findLoanSourceOfFunds())
				.willReturn(Collections.singletonList(new LoanSourceOfFundsDTO()));

		given(settingTransversClient.findLoanRefinanceReason())
				.willReturn(Collections.singletonList(new LoanRefinanceReasonDTO()));

		given(settingTransversClient.findRelationship())
				.willReturn(Collections.singletonList(new RelationshipDTO()));

		given(settingTransversClient.findIndustry())
				.willReturn(Collections.singletonList(new IndustryDTO()));

		given(settingTransversClient.findRoleAbacus())
				.willReturn(Collections.singletonList(new RoleAbacusDTO()));

		doReturn(settingListValuesDTO).when(mapper).map(settingListValues,
				SettingListValuesDTO.class);
		doReturn(settingListValues).when(mapper).map(settingListValuesDTO, SettingListValues.class);
		doReturn(initUserDTO()).when(userClient).find();
		given(settingListValuesRepository.save(any(SettingListValues.class)))
				.willReturn(settingListValues);

		// WHEN
		settingListValuesService.loadSettingFromAbacus();
		// THEN
		verify(settingTransversClient, times(1)).findBranches();
		verify(settingTransversClient, times(1)).findProductLoanReasons();
		verify(settingTransversClient, times(1)).findLoanGuarantorSource();
		verify(settingTransversClient, times(1)).findLoanSourceOfFunds();
		verify(settingTransversClient, times(1)).findLoanRefinanceReason();
		verify(settingTransversClient, times(1)).findRelationship();
		verify(settingTransversClient, times(1)).findIndustry();
		verify(settingTransversClient, times(1)).findRoleAbacus();
	}
}
