<!-- 部门级联列表 -->
<template>
  <div v-show="showable" class="cascader-container">
    <common-select
      v-model="projectType"
      :options="projectTypeEnum.ENUM"
      :all-val="allPT"
      :disabled-val="disabledTypeArr"
      show-option-all
      type="enum"
      size="medium"
      placeholder="项目类型"
      class="project-type-select"
      @change="handleTypeChange"
    />
    <span class="project-cascader-container">
      <el-cascader
        v-model="copyValue"
        :options="options"
        :props="cascaderProps"
        :filterable="props.filterable"
        :clearable="props.clearable"
        :show-all-levels="props.showAllLevels"
        :placeholder="props.placeholder"
        @change="projectChange"
        class="project-cascader"
        style="width: 100%"
      />
      <span @click="handleShowAllClick" class="all-tip pointer" :style="{ color: navbarShowAll ? 'cornflowerblue' : '#dcdfe6' }"> All </span>
    </span>
    <el-tooltip class="item" effect="dark" content="刷新项目列表" placement="right">
      <i v-if="!refreshLoading" class="el-icon-refresh" style="cursor: pointer" @click="refreshProjectList" />
      <i v-else class="el-icon-loading" />
    </el-tooltip>
    <div style="font-size: 13px; margin-left: 15px; color: #333">
      <el-tag v-if="globalProject && globalProject.endDate" type="info" effect="plain">
        完成日期:
        <span v-parse-time="'{y}-{m}-{d}'">{{ globalProject.endDate }}</span>
        | 工期:
        {{ dateDifference(globalProject.startDate, globalProject.endDate) }}天
      </el-tag>
      <el-tag v-if="globalProject && globalProject.businessType" type="info" effect="plain" style="margin-left: 5px">
        {{ businessTypeEnum.VL[globalProject.businessType] }}
      </el-tag>
    </div>
  </div>
</template>

<script setup>
import { defineProps, computed, watch, ref } from 'vue'
import { useStore } from 'vuex'
import { mapGetters } from '@/store/lib'
import { allPT } from '@/settings/config'
import { isNotBlank } from '@data-type/index'
import { projectTypeEnum, businessTypeEnum } from '@enum-ms/contract'

import useUserProjects from '@compos/store/use-user-projects'
import { getBitwiseBack } from '@/utils/data-type/number'
import { dateDifference } from '@/utils/date'

const store = useStore()

const props = defineProps({
  modelValue: {
    type: [Array, Number]
  },
  checkStrictly: {
    // 启用该功能后，可让父子节点取消关联，选择任意一级选项。
    type: Boolean,
    default: false
  },
  expandTrigger: {
    // 次级菜单的展开方式
    type: String,
    default: 'hover'
  },
  emitPath: {
    type: Boolean,
    default: false
  },
  filterable: {
    type: Boolean,
    default: true
  },
  multiple: {
    type: Boolean,
    default: false
  },
  clearable: {
    type: Boolean,
    default: true
  },
  showAllLevels: {
    type: Boolean,
    default: false
  },
  placeholder: {
    type: String,
    default: '可选择项目'
  }
})

const copyValue = ref()
const projectType = ref(allPT)
const refreshLoading = ref(false)
let currentProjectChange = false

const { routeProjectType, currentProjectType, globalProjectId, globalProject, navbarShowAll } = mapGetters([
  'routeProjectType',
  'currentProjectType',
  'globalProjectId',
  'globalProject',
  'navbarShowAll'
])

// 是否显示
const showable = computed(() => isNotBlank(routeProjectType))

const { projectsCascade, processProjects, projects } = useUserProjects()

const options = computed(() => {
  if (navbarShowAll.value) {
    return projectsCascade.value
  }
  return processProjects.value
})

const cascaderProps = computed(() => {
  return {
    value: 'id',
    label: 'name',
    children: 'children',
    checkStrictly: props.checkStrictly,
    expandTrigger: props.expandTrigger,
    emitPath: props.emitPath,
    multiple: props.multiple
  }
})

// 当前目录禁用
const disabledTypeArr = computed(() => {
  const types = []
  if (routeProjectType !== allPT) {
    Object.keys(projectTypeEnum.VL).forEach((v) => {
      if (!(v & routeProjectType)) {
        types.push(v)
      }
    })
  }
  return types
})

watch(
  navbarShowAll,
  (flag) => {
    let isExit = false
    if (flag) {
      isExit = projects.value.some((v) => v.id === copyValue.value)
    } else {
      isExit = processProjects.value.some((v) => v.id === copyValue.value)
    }
    if (!isExit) {
      projectChange(undefined)
    }
  }
  // { immediate: true }
)

watch(
  globalProjectId,
  (val) => {
    if (!currentProjectChange) {
      copyValue.value = val
    }
  },
  { immediate: true }
)

// 监听当前路由的项目类型
watch(
  routeProjectType,
  (val) => {
    // 如果值存在， 并且该值未包含当前项目类型
    if (val && !(val & projectType.value)) {
      // 获取项目类型的种类
      const bitArr = getBitwiseBack(routeProjectType)
      // 如果有多种项目类型，则默认取第一个
      projectType.value = bitArr.length && bitArr.length <= 1 ? val : val & currentProjectType ? currentProjectType : bitArr[0]
      handleTypeChange(projectType.value)
    }
  },
  { immediate: true }
)

// 处理项目类型变更
async function handleTypeChange(val) {
  try {
    // ++this.cascaderKey
    await store.dispatch('project/changeProjectType', val)
  } catch (error) {
    // TODO: 失败应切换回上一个类型
    console.log('切换项目', error)
  }
}

async function projectChange(val) {
  currentProjectChange = true
  try {
    await store.dispatch('project/setProjectId', val)
    // if (val && loginPath.indexOf(this.$route.path) === -1) {
    //   this.refreshSelectedTag()
    // } else {
    //   this.closeAllTags()
    // }
  } catch (error) {
    console.log(error)
  } finally {
    currentProjectChange = false
  }
}

function refreshProjectList() {
  refreshLoading.value = true
  try {
    store.dispatch('project/fetchUserProjects')
  } catch (error) {
    console.log('刷新项目', error)
  } finally {
    refreshLoading.value = false
  }
}

// 显示全部
function handleShowAllClick() {
  store.commit('project/SET_NAVBAR_SHOW_ALL', !navbarShowAll.value)
}
</script>

<style lang="scss" scoped>
.project-cascader-container {
  display: inline-flex;
  position: relative;
  width: 350px;
  .project-cascader {
    width: 100%;
  }

  .all-tip {
    position: absolute;
    right: 5px;
    top: 50%;
    transform: translate(0, -50%);
    border: none;
    user-select: none;
    font-size: 14px;
    margin: 0 5px;
  }

  ::v-deep(.el-input__inner) {
    padding-right: 50px;
  }
  ::v-deep(.el-input__suffix) {
    right: 35px;
  }
  ::v-deep(.el-tag--plain) {
    color: cornflowerblue;
  }
  ::v-deep(.el-tag--plain.el-tag--info) {
    color: var(--el-tag-font-color);
  }
}

.cascader-container {
  display: inline-flex;
  height: 100%;
  align-items: center;
  padding: 10px;
  color: cornflowerblue;
  .project-type-select {
    width: 100px;
  }
  ::v-deep(.el-input input) {
    border: none;
    color: cornflowerblue;
    background-color: unset;
  }
}
</style>
