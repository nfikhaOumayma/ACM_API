/*
 * Copyright (C) TALYS ™ - All Rights Reserved Unauthorized copying of this file, via any medium is
 * strictly prohibited Proprietary and confidential
 */

package com.acm.configuration.activiti;

/**
 * {@link ProcessEngineConfig} class.
 *
 * @author HaythemBenizid
 * @since 0.1.0
 */
// @Configuration
public class ProcessEngineConfig {

	// /** The Constant logger. */
	// private static final Logger logger = LoggerFactory.getLogger(ProcessEngineConfig.class);
	//
	// /** The Constant BPMN_PATH. */
	// private static final String BPMN_PATH = "processes/";
	//
	// /** The data source. */
	// @Autowired
	// private DataSource dataSource;
	//
	// /** The transaction manager. */
	// @Autowired
	// private PlatformTransactionManager platformTransactionManager;

	/**
	 * Process engine configuration.
	 * 
	 * @author HaythemBenizid
	 * @return the spring process engine configuration
	 */
	// @Bean
	// public SpringProcessEngineConfiguration processEngineConfiguration() {
	//
	// SpringProcessEngineConfiguration config = new SpringProcessEngineConfiguration();
	// try {
	// config.setDeploymentResources(getBpmnFiles());
	// }
	// catch (IOException e) {
	// logger.error("Failed to get Bpmn Files");
	// }
	// config.setDataSource(dataSource);
	// config.setTransactionManager(platformTransactionManager);
	// // true : for first time to create DB
	// config.setDatabaseSchemaUpdate("true");
	// config.setHistory("audit");
	// // Async Job Executor
	// config.setAsyncExecutorActivate(true);
	// final DefaultAsyncJobExecutor asyncExecutor = new DefaultAsyncJobExecutor();
	// /*
	// * The maximum number of threads that are created in the threadpool for job execution.
	// * Default value = 10. (This property is only applicable when using the {@link
	// * DefaultAsyncJobExecutor}).
	// */
	// asyncExecutor.setMaxPoolSize(100);
	// /*
	// * The size of the queue on which jobs to be executed are placed, before they are actually
	// * executed. Default value = 100. (This property is only applicable when using the {@link
	// * DefaultAsyncJobExecutor}).
	// */
	// asyncExecutor.setQueueSize(1000);
	//
	// config.setAsyncExecutor(asyncExecutor);
	// return config;
	// }
	//
	// /**
	// * Gets the bpmn files.
	// *
	// * @author HaythemBenizid
	// * @return the bpmn files
	// * @throws IOException Signals that an I/O exception has occurred.
	// */
	// private Resource[] getBpmnFiles() throws IOException {
	//
	// ResourcePatternResolver resourcePatternResolver = new PathMatchingResourcePatternResolver();
	// return resourcePatternResolver.getResources("classpath*:" + BPMN_PATH + "**/*.bpmn");
	// }
	//
	// /**
	// * Repository service.
	// *
	// * @author HaythemBenizid
	// * @param processEngine the process engine
	// * @return the repository service
	// */
	// @Bean
	// public RepositoryService repositoryService(ProcessEngine processEngine) {
	//
	// return processEngine.getRepositoryService();
	// }
	//
	// /**
	// * Identity service.
	// *
	// * @param processEngine the process engine
	// * @return the identity service
	// */
	// @Bean
	// public IdentityService identityService(ProcessEngine processEngine) {
	//
	// return processEngine.getIdentityService();
	// }
	//
	// /**
	// * Form service.
	// *
	// * @author HaythemBenizid
	// * @param processEngine the process engine
	// * @return the form service
	// */
	// @Bean
	// public FormService formService(ProcessEngine processEngine) {
	//
	// return processEngine.getFormService();
	// }
	//
	// /**
	// * Runtime service.
	// *
	// * @author HaythemBenizid
	// * @param processEngine the process engine
	// * @return the runtime service
	// */
	// @Bean
	// public RuntimeService runtimeService(ProcessEngine processEngine) {
	//
	// return processEngine.getRuntimeService();
	// }
	//
	// /**
	// * Task service.
	// *
	// * @author HaythemBenizid
	// * @param processEngine the process engine
	// * @return the task service
	// */
	// @Bean
	// public TaskService taskService(ProcessEngine processEngine) {
	//
	// return processEngine.getTaskService();
	// }
	//
	// /**
	// * Management service.
	// *
	// * @author HaythemBenizid
	// * @param processEngine the process engine
	// * @return the management service
	// */
	// @Bean
	// public ManagementService managementService(ProcessEngine processEngine) {
	//
	// return processEngine.getManagementService();
	// }
	//
	// /**
	// * History service.
	// *
	// * @author HaythemBenizid
	// * @param processEngine the process engine
	// * @return the history service
	// */
	// @Bean
	// public HistoryService historyService(ProcessEngine processEngine) {
	//
	// return processEngine.getHistoryService();
	// }
}
