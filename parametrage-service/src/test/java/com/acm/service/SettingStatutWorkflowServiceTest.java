/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.doReturn;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.List;

import org.assertj.core.api.Assertions;
import org.dozer.DozerBeanMapper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.runner.RunWith;
import org.mockito.InjectMocks;
import org.mockito.Mock;
import org.mockito.MockitoAnnotations;
import org.springframework.data.domain.Sort;
import org.springframework.test.context.junit4.SpringRunner;

import com.acm.constants.common.CommonConstants;
import com.acm.repository.SettingStatutWorkflowRepository;
import com.acm.service.impl.SettingStatutWorkflowServiceImpl;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.SettingGurantorCollateralDTO;
import com.acm.utils.dtos.SettingLevelDTO;
import com.acm.utils.dtos.SettingLevelProcessDTO;
import com.acm.utils.dtos.SettingRequiredStepDTO;
import com.acm.utils.dtos.SettingStatutWorkflowDTO;
import com.acm.utils.models.SettingStatutWorkflow;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link SettingStatutWorkflowServiceTest} class.
 *
 * @author ManelLamloum
 * @since 1.0.8
 */
@RunWith(SpringRunner.class)
public class SettingStatutWorkflowServiceTest {

	/** The setting statut workflow service. */
	@InjectMocks
	SettingStatutWorkflowServiceImpl settingStatutWorkflowService;

	/** The mapper. */
	@Mock
	private DozerBeanMapper mapper;

	/** The setting statut workflow repository. */
	@Mock
	private SettingStatutWorkflowRepository settingStatutWorkflowRepository;

	/** The setting gurantor collateral service. */
	@Mock
	private SettingGurantorCollateralService settingGurantorCollateralService;

	/** The setting level process service. */
	@Mock
	private SettingLevelProcessService settingLevelProcessService;

	/** The setting required step service. */
	@Mock
	private SettingRequiredStepService settingRequiredStepService;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		mapper.setMappingFiles(Collections.singletonList("dozer-configration-mapping.xml"));
	}

	/**
	 * Inits the setting statut workflow DTO.
	 * 
	 * @author ManelLamloum
	 * @return the setting statut workflow DTO
	 */
	private SettingStatutWorkflowDTO initSettingStatutWorkflowDTO() {

		SettingStatutWorkflowDTO settingStatutWorkflowDTO = new SettingStatutWorkflowDTO();
		settingStatutWorkflowDTO.setIsNewLoan(Boolean.TRUE);
		LoanDTO loanDTO = new LoanDTO();
		loanDTO.setProcessName("test");
		loanDTO.setProductId(1);
		loanDTO.setApprovelAmount(new BigDecimal(50000));
		settingStatutWorkflowDTO.setLoanDTO(loanDTO);
		return settingStatutWorkflowDTO;
	}

	/**
	 * Inits the setting gurantor collateral DTO.
	 * 
	 * @author ManelLamloum
	 * @return the setting gurantor collateral DTO
	 */
	private SettingGurantorCollateralDTO initSettingGurantorCollateralDTO() {

		SettingGurantorCollateralDTO settingGurantorCollateralDTO =
				new SettingGurantorCollateralDTO();
		settingGurantorCollateralDTO
				.setCode(CommonConstants.ACM_SETTING_GUARANTOR_COLLATERAL_GUARANTOR);
		return settingGurantorCollateralDTO;
	}

	/**
	 * Inits the setting level process DTO.
	 * 
	 * @author ManelLamloum
	 * @return the setting level process DTO
	 */
	private SettingLevelProcessDTO initSettingLevelProcessDTO() {

		SettingLevelProcessDTO settingLevelProcessDTO = new SettingLevelProcessDTO();
		SettingLevelDTO settingLevelDTO = new SettingLevelDTO();
		settingLevelDTO.setCode("test");
		settingLevelProcessDTO.setSettingLevelDTO(settingLevelDTO);
		return settingLevelProcessDTO;
	}

	/**
	 * Inits the setting required step DTO.
	 * 
	 * @author ManelLamloum
	 * @return the setting required step DTO
	 */
	private SettingRequiredStepDTO initSettingRequiredStepDTO() {

		SettingRequiredStepDTO settingRequiredStepDTO = new SettingRequiredStepDTO();
		settingRequiredStepDTO.setCode(CommonConstants.ACM_SETTING_REQUIRED_STEP_FIELD_VISIT);
		return settingRequiredStepDTO;
	}

	/**
	 * Should return list setting statut workflow DTO.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldReturnListSettingStatutWorkflowDTO() {

		// GIVEN
		given(settingStatutWorkflowRepository.findAll(any(BooleanBuilder.class), any(Sort.class)))
				.willReturn(Collections.singletonList(new SettingStatutWorkflow()));
		doReturn(new SettingStatutWorkflowDTO()).when(mapper).map(new SettingStatutWorkflow(),
				SettingStatutWorkflowDTO.class);
		given(settingGurantorCollateralService.find(any(SettingGurantorCollateralDTO.class)))
				.willReturn(Collections.singletonList(initSettingGurantorCollateralDTO()));
		given(settingLevelProcessService.find(any(SettingLevelProcessDTO.class)))
				.willReturn(Collections.singletonList(initSettingLevelProcessDTO()));
		given(settingRequiredStepService.find(any(SettingRequiredStepDTO.class)))
				.willReturn(Collections.singletonList(initSettingRequiredStepDTO()));
		// WHEN
		List<SettingStatutWorkflowDTO> result =
				settingStatutWorkflowService.find(initSettingStatutWorkflowDTO());
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success find all.
	 * 
	 * @author ManelLamloum
	 */
	@Test
	void shouldSuccessFindAll() {

		// GIVEN
		given(settingStatutWorkflowRepository.findAll())
				.willReturn(Collections.singletonList(new SettingStatutWorkflow()));
		doReturn(new SettingStatutWorkflowDTO()).when(mapper).map(new SettingStatutWorkflow(),
				SettingStatutWorkflowDTO.class);

		// WHEN

		List<SettingStatutWorkflowDTO> result =
				settingStatutWorkflowService.findAll(new SettingStatutWorkflowDTO());
		// THEN
		Assertions.assertThat(result).isNotNull();
	}

}
