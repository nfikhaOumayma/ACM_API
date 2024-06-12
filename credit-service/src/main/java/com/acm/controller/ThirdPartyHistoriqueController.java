/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.controller;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.acm.exceptions.type.GEDException;
import com.acm.exceptions.type.NationalIdNotFoundException;
import com.acm.exceptions.type.ResourcesNotFoundException;
import com.acm.service.ThirdPartyHistoriqueService;
import com.acm.utils.dtos.LoanDTO;
import com.acm.utils.dtos.ThirdPartyHistoriqueDTO;
import com.acm.utils.enums.ThirdPartyCategory;

/**
 * This class @{link ThirdPartyHistoriqueController} used to control all the ThirdPartyHistorique
 * requests.
 *
 * @author HaythemBenizid
 * @since 1.0.11
 */
@RestController
@RequestMapping("/third-party-historiques")
public class ThirdPartyHistoriqueController {

	/** The ThirdPartyHistorique service. */
	@Autowired
	private ThirdPartyHistoriqueService thirdPartyHistoriqueService;

	/**
	 * Find ThirdPartyHistoriqueF by id.
	 *
	 * @author HaythemBenizid
	 * @param id the id
	 * @return the thirdPartyHistorique DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/{id}")
	public ThirdPartyHistoriqueDTO findById(@PathVariable("id") Long id)
			throws ResourcesNotFoundException {

		return thirdPartyHistoriqueService.find(id);
	}

	/**
	 * Find by given params.
	 * 
	 * @author HaythemBenizid
	 * @param thirdPartyHistoriqueDTO the third party historique DTO
	 * @return the list
	 */
	@PostMapping("/")
	public List<ThirdPartyHistoriqueDTO> find(
			@RequestBody ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO) {

		return thirdPartyHistoriqueService.find(thirdPartyHistoriqueDTO);
	}

	/**
	 * Find for screening: find last stored data for all ThirdPartyCategory by id customer.
	 *
	 * @author HaythemBenizid
	 * @param thirdPartyHistoriqueDTO the third party historique DTO
	 * @return the list
	 * @throws NationalIdNotFoundException the national id not found exception
	 */
	@PostMapping("/find-for-screening")
	public List<ThirdPartyHistoriqueDTO> findForScreening(
			@RequestBody ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO)
			throws NationalIdNotFoundException {

		return thirdPartyHistoriqueService.findForScreening(thirdPartyHistoriqueDTO);
	}

	/**
	 * Find ThirdPartyHistorique by given {@link ThirdPartyCategory} and ID Loan and ID customer for
	 * given category =(CUSTOMER / GUARANTOR).
	 *
	 * @author HaythemBenizid
	 * @param category the category
	 * @param idLoan the id loan
	 * @param idCustomer the id customer
	 * @param categoryCustomer the category customer
	 * @return the list
	 */
	@GetMapping("/{category}/{idLoan}/{idCustomer}/{categoryCustomer}")
	public List<ThirdPartyHistoriqueDTO> find(@PathVariable("category") String category,
			@PathVariable("idLoan") Long idLoan, @PathVariable("idCustomer") Long idCustomer,
			@PathVariable("categoryCustomer") String categoryCustomer) {

		return thirdPartyHistoriqueService.find(category, idLoan, idCustomer, categoryCustomer);
	}

	/**
	 * Generate I-SCORE report : Call I-SCORE API API.
	 *
	 * @author HaythemBenizid
	 * @param thirdPartyHistoriqueDTO the third party historique DTO
	 * @return the byte[]
	 * @throws ResourcesNotFoundException the resources not found exception
	 * @throws GEDException the GED exception
	 */
	@PostMapping("/report-iscore")
	public byte[] generateIScoreReport(@RequestBody ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO)
			throws ResourcesNotFoundException, GEDException {

		return thirdPartyHistoriqueService.generateIscoreReport(thirdPartyHistoriqueDTO);
	}

	/**
	 * Generate I-SCORE stored report in DB.
	 *
	 * @author HaythemBenizid
	 * @param thirdPartyHistoriqueDTO the third party historique DTO
	 * @return the byte[]
	 */
	@PostMapping("/report-iscore-stored")
	public byte[] generateIscoreStoredReport(
			@RequestBody ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO) {

		return thirdPartyHistoriqueService.generateIscoreStoredReport(thirdPartyHistoriqueDTO);
	}

	/**
	 * Validate I-score + AML.
	 *
	 * @author yesser somai
	 * @param thirdPartyHistoriqueDTO the third party historique DTO
	 * @return the third party historique DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/validate")
	public ThirdPartyHistoriqueDTO validate(
			@RequestBody ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO)
			throws ResourcesNotFoundException {

		return thirdPartyHistoriqueService.validate(thirdPartyHistoriqueDTO);
	}

	/**
	 * Save.
	 *
	 * @param thirdPartyHistoriqueDTO the third party historique DTO
	 * @return the third party historique DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/save")
	public ThirdPartyHistoriqueDTO save(
			@RequestBody ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO)
			throws ResourcesNotFoundException {

		return thirdPartyHistoriqueService.save(thirdPartyHistoriqueDTO);
	}

	/**
	 * Save with token.
	 *
	 * @param thirdPartyHistoriqueDTO the third party historique DTO
	 * @param token the token
	 * @return the third party historique DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@PostMapping("/saveWithToken")
	public ThirdPartyHistoriqueDTO saveWithToken(
			@RequestBody ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO,
			@RequestHeader("Authorization") String token) throws ResourcesNotFoundException {

		return thirdPartyHistoriqueService.save(thirdPartyHistoriqueDTO);

	}

	/**
	 * Creates the.
	 *
	 * @param thirdPartyHistoriqueDTO the third party historique DTO
	 * @return the third party historique DTO
	 */
	@PostMapping("/create")
	public ThirdPartyHistoriqueDTO create(
			@RequestBody ThirdPartyHistoriqueDTO thirdPartyHistoriqueDTO) {

		return thirdPartyHistoriqueService.save(thirdPartyHistoriqueDTO);
	}

	/**
	 * Find loan third party historique with token.
	 *
	 * @param idLoan the id loan
	 * @return the loan DTO
	 */

	@GetMapping("/idLoan/{idLoan}")
	public List<ThirdPartyHistoriqueDTO> findThirdPartyByIdLoan(
			@PathVariable("idLoan") Long idLoan) {

		return thirdPartyHistoriqueService.findByIdLoan(idLoan);
	}

	/**
	 * Find loan third party historique with token.
	 *
	 * @param searchQueryId the search query id
	 * @param category the category
	 * @param token the token
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/find-with-token/by-search-query-id/{searchQueryId}/{category}")
	LoanDTO findLoanThirdPartyHistoriqueWithToken(@PathVariable("searchQueryId") Long searchQueryId,
			@PathVariable("category") String category, @RequestHeader("Authorization") String token)
			throws ResourcesNotFoundException {

		return thirdPartyHistoriqueService.findBySearchQueryIdAndCategory(searchQueryId, category);
	}

	/**
	 * Find loan third party historique by customer reis with token.
	 *
	 * @param customerIdReis the customer id reis
	 * @param category the category
	 * @param token the token
	 * @return the loan DTO
	 * @throws ResourcesNotFoundException the resources not found exception
	 */
	@GetMapping("/find-with-token/by-customer-reis-id/{customerIdReis}/{category}")
	LoanDTO findLoanThirdPartyHistoriqueByCustomerReisWithToken(
			@PathVariable("customerIdReis") Long customerIdReis,
			@PathVariable("category") String category, @RequestHeader("Authorization") String token)
			throws ResourcesNotFoundException {

		return thirdPartyHistoriqueService.findByCustomerIdReisAndCategory(customerIdReis,
				category);

	}

}
