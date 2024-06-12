/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.service;

import static org.assertj.core.api.Assertions.assertThat;
import static org.hamcrest.CoreMatchers.containsString;
import static org.junit.Assert.assertThat;
import static org.mockito.ArgumentMatchers.any;
import static org.mockito.BDDMockito.given;
import static org.mockito.Mockito.times;
import static org.mockito.Mockito.verify;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Optional;
import java.util.function.Function;

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
import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.domain.Sort;
import org.springframework.test.context.junit4.SpringRunner;

import com.acm.client.ParametrageClient;
import com.acm.client.UserClient;
import com.acm.constants.common.CommonExceptionsMessage;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.repository.AcmDocumentsRepository;
import com.acm.service.impl.AcmDocumentsServiceImpl;
import com.acm.utils.dtos.AcmDocumentsDTO;
import com.acm.utils.dtos.CustomerDTO;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.LoansDocumentsDTO;
import com.acm.utils.dtos.SettingDocumentTypeDTO;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.dtos.pagination.AcmDocumentsPaginationDTO;
import com.acm.utils.models.AcmDocuments;
import com.acm.utils.models.SettingDocumentType;
import com.querydsl.core.BooleanBuilder;

/**
 * {@link AcmDocumentsServiceTest} class.
 *
 * @author HaythemBenizid
 * @since 0.2.0
 */
@RunWith(SpringRunner.class)
class AcmDocumentsServiceTest {

	/** The documentsLoan service. */
	@InjectMocks
	private AcmDocumentsServiceImpl documentsLoanService;

	/** The documentsLoan repository. */
	@Mock
	private AcmDocumentsRepository documentsLoanRepository;

	/** The loan service. */
	@Mock
	private LoanService loanService;

	/** The customer service. */
	@Mock
	private CustomerService customerService;

	/** The mapper. */
	@Spy
	private DozerBeanMapper mapper;

	/** The user client. */
	@Mock
	private UserClient userClient;

	/** The parametrage client. */
	@Mock
	private ParametrageClient parametrageClient;

	/** The environment. */
	@Mock
	private Environment environment;

	/**
	 * Sets the up.
	 */
	@BeforeEach
	void setUp() {

		MockitoAnnotations.initMocks(this);
		mapper.setMappingFiles(Collections.singletonList("dozer-configration-mapping.xml"));
	}

	/**
	 * Initialize a new UserDTO.
	 * 
	 * @return a new UserDTO
	 */
	private UserDTO initUserDTO() {

		UserDTO userDTO = new UserDTO();
		userDTO.setNom("test");
		userDTO.setEmail("test@test.gmail");
		userDTO.setLogin("login");
		userDTO.setPrenom("benTest");
		return userDTO;
	}

	/**
	 * Should success find documentsLoan.
	 * 
	 * @author HaythemBenizid
	 */
	@Test
	void shouldSuccessFindAcmDocumentsLoan() {

		// GIVEN
		AcmDocuments documentsLoan = initAcmDocumentsLoan();
		AcmDocumentsDTO documentsLoanDTO = initAcmDocumentsLoanDTO();
		given(documentsLoanRepository.findAll())
				.willReturn(Collections.singletonList(documentsLoan));
		given(mapper.map(documentsLoan, AcmDocumentsDTO.class)).willReturn(documentsLoanDTO);

		// WHEN
		List<AcmDocumentsDTO> result = documentsLoanService.find(documentsLoanDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success find acm documents loan pagination.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessFindAcmDocumentsLoanPagination() throws ResourcesNotFoundException {

		// GIVEN
		AcmDocumentsPaginationDTO acmDocumentsPaginationDTO = new AcmDocumentsPaginationDTO();
		acmDocumentsPaginationDTO.setParams(new AcmDocumentsDTO());
		AcmDocumentsDTO documentsLoanDTO = initAcmDocumentsLoanDTO();
		documentsLoanDTO.setIdCustomer(1L);
		AcmDocuments documentsLoan = initAcmDocumentsLoan();
		Page<AcmDocuments> pagedResult = new Page<AcmDocuments>() {

			@Override
			public Iterator<AcmDocuments> iterator() {

				return null;
			}

			@Override
			public Pageable previousPageable() {

				return null;
			}

			@Override
			public Pageable nextPageable() {

				return null;
			}

			@Override
			public boolean isLast() {

				return false;
			}

			@Override
			public boolean isFirst() {

				return false;
			}

			@Override
			public boolean hasPrevious() {

				return false;
			}

			@Override
			public boolean hasNext() {

				return false;
			}

			@Override
			public boolean hasContent() {

				return true;
			}

			@Override
			public Sort getSort() {

				return null;
			}

			@Override
			public int getSize() {

				return 0;
			}

			@Override
			public int getNumberOfElements() {

				return 0;
			}

			@Override
			public int getNumber() {

				return 0;
			}

			@Override
			public List<AcmDocuments> getContent() {

				return Collections.singletonList(documentsLoan);
			}

			@Override
			public <U> Page<U> map(Function<? super AcmDocuments, ? extends U> converter) {

				return null;
			}

			@Override
			public int getTotalPages() {

				return 0;
			}

			@Override
			public long getTotalElements() {

				return 0;
			}
		};
		given(documentsLoanRepository.findAll(any(BooleanBuilder.class), any(Pageable.class)))
				.willReturn(pagedResult);
		given(mapper.map(documentsLoan, AcmDocumentsDTO.class)).willReturn(documentsLoanDTO);
		given(userClient.find()).willReturn(initUserDTO());
		given(loanService.findByOwners()).willReturn(new ArrayList<>());
		given(userClient.find()).willReturn(initUserDTO());
		given(customerService.findCustomer(1L)).willReturn(new CustomerDTO());

		// WHEN
		AcmDocumentsPaginationDTO result = documentsLoanService.find(acmDocumentsPaginationDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success save.
	 * 
	 * @author HaythemBenizid
	 */
	@Test
	void shouldSuccessSave() {

		// GIVEN
		AcmDocumentsDTO documentsLoanDTO = initAcmDocumentsLoanDTO();
		AcmDocuments documentsLoan = initAcmDocumentsLoan();
		given(documentsLoanRepository.save(any(AcmDocuments.class))).willReturn(documentsLoan);
		given(mapper.map(documentsLoan, AcmDocumentsDTO.class)).willReturn(documentsLoanDTO);
		given(userClient.find()).willReturn(initUserDTO());
		// WHEN
		AcmDocumentsDTO documentsLoanDTOReponse = documentsLoanService.save(documentsLoanDTO);

		// THEN
		assertThat(documentsLoanDTOReponse).isNotNull();
	}

	/**
	 * Should success update.
	 * 
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessUpdate() throws ResourcesNotFoundException {

		// GIVEN
		AcmDocumentsDTO documentsLoanDTO = initAcmDocumentsLoanDTO();
		documentsLoanDTO.setIdDocument(new Long(1));
		AcmDocuments documentsLoan = mapper.map(documentsLoanDTO, AcmDocuments.class);
		documentsLoan.setAcmVersion(0);
		given(documentsLoanRepository.findById(any(Long.class)))
				.willReturn(Optional.of(documentsLoan));
		given(documentsLoanRepository.save(any(AcmDocuments.class))).willReturn(documentsLoan);
		given(mapper.map(documentsLoan, AcmDocumentsDTO.class)).willReturn(documentsLoanDTO);
		given(userClient.find()).willReturn(initUserDTO());

		// WHEN
		AcmDocumentsDTO documentsLoanDTOReponse =
				documentsLoanService.save(documentsLoan.getIdDocument(), documentsLoanDTO);

		// THEN
		assertThat(documentsLoanDTOReponse).isNotNull();
	}

	/**
	 * Should success find acm documents loan by ID.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessFindAcmDocumentsLoanByID() throws ResourcesNotFoundException {

		// GIVEN
		given(documentsLoanRepository.findById(any(Long.class)))
				.willReturn(Optional.of(initAcmDocumentsLoan()));

		// WHEN
		AcmDocumentsDTO documentsLoanDTO = documentsLoanService.find(new Long(1));

		// THEN
		assertThat(documentsLoanDTO).isNotNull();
	}

	/**
	 * Should not success find acm documents loan by ID.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldNotSuccessFindAcmDocumentsLoanByID() throws ResourcesNotFoundException {

		// GIVEN
		given(environment.getProperty(any(String.class)))
				.willReturn(CommonExceptionsMessage.EXCEPTIONS_OBJECT_NULL);
		// WHEN
		try {
			documentsLoanService.find(new Long(1));
			Assert.fail("ResourcesNotFoundException expected");
		}
		catch (ResourcesNotFoundException expected) {
			String message = expected.getMessage();
			assertThat(message, containsString("AcmDocuments with ID"));
		}
		catch (Exception anyOther) {
			Assert.fail("Unexpected Exception");
		}
	}

	/**
	 * Should success delete.
	 * 
	 * @author HaythemBenizid
	 */
	@Test
	void shouldSuccessDelete() {

		// GIVEN
		AcmDocumentsDTO documentsLoanDTO = initAcmDocumentsLoanDTO();
		documentsLoanDTO.setIdDocument(new Long(1));

		// WHEN
		documentsLoanService.delete(documentsLoanDTO);

		// THEN
		verify(documentsLoanRepository, times(1)).deleteById(any(Long.class));
	}

	/**
	 * Should success disable document.
	 *
	 * @author HaythemBenizid
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@Test
	void shouldSuccessDisableDocument() throws ResourcesNotFoundException {

		// GIVEN
		AcmDocumentsDTO documentsLoanDTO = initAcmDocumentsLoanDTO();
		documentsLoanDTO.setIdCustomer(1L);
		AcmDocuments documentsLoan = initAcmDocumentsLoan();
		documentsLoan.setIdCustomer(1L);
		given(documentsLoanRepository.save(any(AcmDocuments.class))).willReturn(documentsLoan);
		given(mapper.map(documentsLoan, AcmDocumentsDTO.class)).willReturn(documentsLoanDTO);
		given(userClient.find()).willReturn(initUserDTO());
		given(documentsLoanRepository.findById(any(Long.class)))
				.willReturn(Optional.of(initAcmDocumentsLoan()));
		given(documentsLoanService.find(new Long(1))).willReturn(documentsLoanDTO);
		// WHEN
		documentsLoanService.disableDocument(documentsLoanDTO);

		// THEN
	}

	/**
	 * Should success find loans documents by customer.
	 *
	 * @author HaythemBenizid
	 */
	@SuppressWarnings("unchecked")
	@Test
	void shouldSuccessFindLoansDocumentsByCustomer() {

		// GIVEN
		AcmDocumentsDTO documentsLoanDTO = initAcmDocumentsLoanDTO();
		documentsLoanDTO.setIdCustomer(1L);
		AcmDocuments documentsLoan = initAcmDocumentsLoan();
		documentsLoan.setIdCustomer(1L);
		LoanDTO loanDTO = new LoanDTO(1L);
		SettingDocumentTypeDTO settingDocumentTypeDTO = new SettingDocumentTypeDTO();
		settingDocumentTypeDTO.setCategorie(1);
		settingDocumentTypeDTO.setId(1L);
		List<SettingDocumentTypeDTO> list = new ArrayList<>(Arrays.asList(settingDocumentTypeDTO));
		SettingDocumentType settingDocumentType = new SettingDocumentType();
		settingDocumentType.setCategorie(1);
		settingDocumentType.setId(1L);
		documentsLoanDTO.setSettingDocumentTypeDTO(settingDocumentTypeDTO);

		given(documentsLoanRepository.findByLoanIdAndEnabledAndSettingDocumentTypeIn(
				any(Long.class), any(Boolean.class), (List<SettingDocumentType>) any(Object.class)))
						.willReturn(Collections.singletonList(documentsLoan));
		given(loanService.findByIdCustomer(any(Long.class)))
				.willReturn(Collections.singletonList(loanDTO));
		given(parametrageClient.find(any(SettingDocumentTypeDTO.class))).willReturn(list);
		given(mapper.map(documentsLoan, AcmDocumentsDTO.class)).willReturn(documentsLoanDTO);
		given(mapper.map(settingDocumentTypeDTO, SettingDocumentType.class))
				.willReturn(settingDocumentType);

		// WHEN
		List<LoansDocumentsDTO> result =
				documentsLoanService.findLoansDocumentsByCustomer(documentsLoanDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Should success find expenses document.
	 *
	 * @author HaythemBenizid
	 */
	@Test
	void shouldSuccessFindExpensesDocument() {

		// GIVEN
		AcmDocuments documentsLoan = initAcmDocumentsLoan();
		AcmDocumentsDTO documentsLoanDTO = initAcmDocumentsLoanDTO();
		documentsLoanDTO.setExpensesId(1L);
		given(documentsLoanRepository.findAll())
				.willReturn(Collections.singletonList(documentsLoan));
		given(mapper.map(documentsLoan, AcmDocumentsDTO.class)).willReturn(documentsLoanDTO);

		// WHEN
		List<AcmDocumentsDTO> result = documentsLoanService.findExpensesDocument(documentsLoanDTO);

		// THEN
		Assertions.assertThat(result).isNotNull();
	}

	/**
	 * Inits the acm documents loan.
	 * 
	 * @author HaythemBenizid
	 * @return the acm documents
	 */
	private AcmDocuments initAcmDocumentsLoan() {

		AcmDocuments documentsLoan = new AcmDocuments();
		documentsLoan.setAcmVersion(0);
		return documentsLoan;
	}

	/**
	 * Inits the acm documents loan DTO.
	 * 
	 * @author HaythemBenizid
	 * @return the acm documents DTO
	 */
	private AcmDocumentsDTO initAcmDocumentsLoanDTO() {

		AcmDocumentsDTO documentsLoanDTO = new AcmDocumentsDTO();
		documentsLoanDTO.setIdDocument(new Long(1));
		documentsLoanDTO.setTitre("Test Title 1");
		documentsLoanDTO.setDescription("Test Desc 1");
		documentsLoanDTO.setLoanId(new Long(1));
		return documentsLoanDTO;
	}
}
