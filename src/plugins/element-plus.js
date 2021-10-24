import {
  // ElAlert,
  // ElAside,
  // ElAutocomplete,
  // ElAvatar,
  // ElBacktop,
  ElBadge,
  ElBreadcrumb,
  ElBreadcrumbItem,
  // ElButton,  // 非必要不使用ElButton，在禁用后点击按钮内文字仍可触发点击事件（使用common-button）
  ElButtonGroup,
  // ElCalendar,
  ElCard,
  ElCarousel,
  ElCarouselItem,
  ElCascader,
  ElCascaderPanel,
  ElCheckbox,
  ElCheckboxButton,
  ElCheckboxGroup,
  ElCol,
  ElCollapse,
  ElCollapseItem,
  ElCollapseTransition,
  ElColorPicker,
  ElContainer,
  ElDatePicker,
  ElDialog,
  ElDivider,
  ElDrawer,
  ElDropdown,
  ElDropdownItem,
  ElDropdownMenu,
  ElFooter,
  ElForm,
  ElFormItem,
  ElHeader,
  ElIcon,
  ElImage,
  ElInput,
  ElInputNumber,
  ElLink,
  ElMain,
  // ElMenu,
  // ElMenuItem,
  // ElMenuItemGroup,
  ElOption,
  ElOptionGroup,
  ElPageHeader,
  ElPagination,
  ElPopconfirm,
  ElPopover,
  // ElPopper,
  ElProgress,
  ElRadio,
  ElRadioButton,
  ElRadioGroup,
  ElRate,
  ElRow,
  // ElScrollbar,
  ElSelect,
  ElSlider,
  ElStep,
  ElSteps,
  // ElSubMenu,
  ElSwitch,
  ElTabPane,
  ElTable,
  ElTableColumn,
  ElTabs,
  ElTag,
  // ElTimePicker,
  // ElTimeSelect,
  // ElTimeline,
  // ElTimelineItem,
  ElTooltip,
  // ElTransfer,
  ElLoading,
  ElTree,
  ElUpload
  // ElMessage,
  // ElMessageBox
} from 'element-plus'

const components = [
  // ElAlert,
  // ElAside,
  // ElAutocomplete,
  // ElAvatar,
  // ElBacktop,
  ElBadge,
  ElBreadcrumb,
  ElBreadcrumbItem,
  // ElButton,
  ElButtonGroup,
  // ElCalendar,
  ElCard,
  ElCarousel,
  ElCarouselItem,
  ElCascader,
  ElCascaderPanel,
  ElCheckbox,
  ElCheckboxButton,
  ElCheckboxGroup,
  ElCol,
  ElCollapse,
  ElCollapseItem,
  ElCollapseTransition,
  ElColorPicker,
  ElContainer,
  ElDatePicker,
  ElDialog,
  ElDivider,
  ElDrawer,
  ElDropdown,
  ElDropdownItem,
  ElDropdownMenu,
  ElFooter,
  ElForm,
  ElFormItem,
  ElHeader,
  ElIcon,
  ElImage,
  ElInput,
  ElInputNumber,
  ElLink,
  ElMain,
  // ElMenu,
  // ElMenuItem,
  // ElMenuItemGroup,
  ElOption,
  ElOptionGroup,
  ElPageHeader,
  ElPagination,
  ElPopconfirm,
  ElPopover,
  // ElPopper,
  ElProgress,
  ElRadio,
  ElRadioButton,
  ElRadioGroup,
  ElRate,
  ElRow,
  // ElScrollbar,
  ElSelect,
  ElSlider,
  ElStep,
  ElSteps,
  // ElSubMenu,
  ElSwitch,
  ElTabPane,
  ElTable,
  ElTableColumn,
  ElTabs,
  ElTag,
  // ElTimePicker,
  // ElTimeSelect,
  // ElTimeline,
  // ElTimelineItem,
  ElTooltip,
  // ElTransfer,
  ElTree,
  ElUpload
]

const plugins = [
//   ElInfiniteScroll,
  ElLoading
  // ElMessage,
  // ElMessageBox
//   ElNotification
]

// 配置
const option = { size: 'small', zIndex: 3000 }

const useElementPlus = (app) => {
  // element全局配置
  app.config.globalProperties.$ELEMENT = option

  // 组件注册
  components.forEach((component) => {
    app.component(component.name, component)
  })

  // 插件注册
  plugins.forEach((plugin) => {
    app.use(plugin)
  })
}

export default useElementPlus
