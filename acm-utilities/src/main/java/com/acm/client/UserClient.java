/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */
package com.acm.client;

import java.util.List;

import org.springframework.cloud.netflix.ribbon.RibbonClient;
import org.springframework.cloud.openfeign.FeignClient;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;

import com.acm.configuration.feignclient.ClientConfiguration;
import com.acm.configuration.feignclient.LoadbalancerRuleFeignConfiguration;
import com.acm.utils.dtos.UserDTO;
import com.acm.utils.enums.UserHierarchicalType;

/**
 * The {@link UserClient} Interface. to inject in order to consume services from
 * authentication-service
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
@FeignClient(value = "authentication-service", configuration = ClientConfiguration.class,
		decode404 = true)
@RibbonClient(name = "authentication-service",
		configuration = LoadbalancerRuleFeignConfiguration.class)
public interface UserClient {

	/**
	 * Find : returning connected user name.
	 * 
	 * @author HaythemBenizid
	 * @return the user {@link UserDTO}
	 */
	@RequestMapping("/users/connected")
	UserDTO find();

	/**
	 * Find by login.
	 * 
	 * @author HaythemBenizid
	 * @param login the login
	 * @return the user DTO
	 */
	@GetMapping("/users/{login}")
	UserDTO findByLogin(@PathVariable("login") String login);

	/**
	 * Find list user by given responsable.
	 * 
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @return the list
	 */
	@PostMapping("/users/")
	List<UserDTO> find(@RequestBody UserDTO userDTO);

	/**
	 * Create user method : used by the BATCH to save USERS in ACM DB.
	 *
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @param token the token
	 * @return the user DTO
	 */
	@PostMapping("/users/create-for-batch")
	UserDTO createForBatch(@RequestBody UserDTO userDTO,
			@RequestHeader("Authorization") String token);

	/**
	 * Create IB user method.
	 *
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @return the user DTO
	 */
	@PostMapping("/users/create-for-ib")
	UserDTO createForIB(@RequestBody UserDTO userDTO);

	/**
	 * Find list user (ALL {@link UserHierarchicalType}) by given user.
	 * 
	 * @author MoezMhiri
	 * @return the list
	 */
	@RequestMapping("/users/findUsers")
	List<UserDTO> findUsers();

	/**
	 * Find list user by given branch id.
	 * 
	 * @author MoezMhiri
	 * @return the list
	 */
	@RequestMapping("/users/findUsersBranch")
	List<UserDTO> findUsersByBranchId();

	/**
	 * Find user by given by groupe Code.
	 *
	 * @author HaythemBenizid
	 * @param userDTO the user DTO
	 * @return the list
	 */
	@PostMapping("/users/find-by-groupe")
	List<UserDTO> findByGroupe(@RequestBody UserDTO userDTO);

	/**
	 * Find by groupe code and branch ID.
	 *
	 * @author Yesser Somai
	 * @param codeGroupe the code groupe
	 * @param branchId the branch id
	 * @return the list
	 */
	@GetMapping("/users/find-by-groupe-branch/{codeGroupe}/{branchId}")
	List<UserDTO> findByGroupeCodeAndBranchID(@PathVariable String codeGroupe,
			@PathVariable Integer branchId);

	/**
	 * Find by groupe code and branch ID and access branches.
	 * 
	 * @author idridi
	 * @param codeGroupe the code groupe
	 * @param branchId the branch id
	 * @return the list
	 */
	@GetMapping("/users/find-by-groupe-branch-access-branches/{codeGroupe}/{branchId}")
	List<UserDTO> findByGroupeCodeAndBranchIDAndAccessBranches(@PathVariable String codeGroupe,
			@PathVariable Integer branchId);

	/**
	 * Gets the users with loan branch in their access branches.
	 *
	 * @author ManelLamloum
	 * @param loanBranchId the loan branch id
	 * @param usersDTO the users DTO
	 * @return the users with loan branch in their access branches
	 */
	@PostMapping("/users/get-users-with-loanBranch-in-their-accessBranches/{loanBranchId}")
	List<UserDTO> getUsersWithLoanBranchInTheirAccessBranches(@PathVariable Integer loanBranchId,
			@RequestBody List<UserDTO> usersDTO);

	/**
	 * Gets the responsible of user.
	 *
	 * @author ManelLamloum
	 * @param usersDTO the users DTO
	 * @return the responsible of user
	 */
	@PostMapping("/users/get-responsible-of-user")
	UserDTO findResponsibleOfUser(@RequestBody UserDTO usersDTO);

	/**
	 * Count user.
	 *
	 * @author kouali
	 * @return the integer
	 */
	@GetMapping("/users/count-user")
	Integer countUser();

	/**
	 * Gets the simultanious user.
	 *
	 * @return the simultanious user
	 */
	@GetMapping("/users/simultanious-user")
	Long getSimultaniousUser();

}
