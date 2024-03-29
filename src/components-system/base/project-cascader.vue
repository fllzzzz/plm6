<!-- 部门级联列表 -->
<template>
  <span class="project-cascader-container">
    <el-cascader
      v-model="copyValue"
      :options="options"
      :props="cascaderProps"
      :collapse-tags="collapseTags"
      :filterable="props.filterable"
      :clearable="props.clearable"
      :disabled="props.disabled"
      :show-all-levels="props.showAllLevels"
      :placeholder="props.placeholder"
      @change="handleChange"
      class="project-cascader"
      style="width:100%"
    />
    <span @click="showAll = !showAll" class="all-tip" :style="{color: showAll ? 'cornflowerblue' : '#c1c2c5'}"> All </span>
  </span>
</template>

<script setup>
import { defineEmits, defineProps, computed, watch, ref } from 'vue'
import { allPT } from '@/settings/config'
import { isBlank, judgeSameValue } from '@data-type/index'
import useUserProjects from '@compos/store/use-user-projects'

const emit = defineEmits(['update:modelValue', 'change'])

const props = defineProps({
  modelValue: {
    type: [Array, Number]
  },
  projectType: {
    // 项目类型
    type: Number,
    default: allPT // 全部
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
  collapseTags: {
    // 多选模式下是否折叠Tag
    type: Boolean,
    default: false
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
    default: false
  },
  disabled: {
    type: Boolean,
    default: false
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
const showAll = ref(false)

const cascaderProps = computed(() => {
  return {
    value: 'id',
    label: 'label',
    children: 'children',
    checkStrictly: props.checkStrictly,
    expandTrigger: props.expandTrigger,
    emitPath: props.emitPath,
    multiple: props.multiple
  }
})

watch(
  () => props.modelValue,
  (value) => {
    if (value instanceof Array) {
      copyValue.value = [...value]
    } else {
      copyValue.value = value
    }
    handleChange(value)
  },
  { immediate: true }
)

const { projectsCascade, processProjects, projects } = useUserProjects()

const options = computed(() => {
  if (showAll.value) {
    return projectsCascade.value
  }
  return processProjects.value
})

watch(
  showAll,
  (displayable) => {
    let cv = copyValue.value
    if (Array.isArray(copyValue.value)) {
      if (displayable) {
        cv = projects.value.map(v => v.id).filter(v => copyValue.value.includes(v))
      } else {
        cv = processProjects.value.map(v => v.id).filter(v => copyValue.value.includes(v))
      }
    } else {
      let isExit = false
      if (displayable) {
        isExit = projects.value.some(v => v.id === copyValue.value)
      } else {
        isExit = processProjects.value.some(v => v.id === copyValue.value)
      }
      if (!isExit) {
        cv = undefined
      }
    }

    handleChange(cv)
  },
  { immediate: true }
)

// 发生change
function handleChange(val) {
  // 发生变化
  const isChange = !judgeSameValue(val, props.modelValue)
  // 两个值都为空
  const allBlank = isBlank(val) && isBlank(props.modelValue)

  if (isChange && !allBlank) {
    emit('update:modelValue', val)
    emit('change', val)
    return true
  }
  return false
}
</script>

<style lang="scss" scoped>
.project-cascader-container {
  display: inline-flex;
  position: relative;

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
  ::v-deep(.el-input__suffix){
    right: 35px;
  }
}
</style>
