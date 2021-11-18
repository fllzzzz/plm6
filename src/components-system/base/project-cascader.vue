<!-- 部门级联列表 -->
<template>
  <span class="project-cascader-container">
    <el-cascader
      v-model="copyValue"
      :options="options"
      :props="cascaderProps"
      :filterable="props.filterable"
      :clearable="props.clearable"
      :show-all-levels="props.showAllLevels"
      :placeholder="props.placeholder"
      @change="handleChange"
      class="project-cascader"
      style="width:100%"
    />
    <el-tag
      :type="showAll ? undefined : 'info'"
      size="medium"
      effect="plain"
      @click="showAll = !showAll"
      class="all-tag"
    >
      All
    </el-tag>
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
  emitPath: {
    type: Boolean,
    default: false
  },
  filterable: {
    type: Boolean,
    default: false
  },
  multiple: {
    type: Boolean,
    default: false
  },
  clearable: {
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
    label: 'name',
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
  (flag) => {
    let isExit = false
    if (flag) {
      isExit = projects.value.some(v => v.id === copyValue.value)
    } else {
      isExit = processProjects.value.some(v => v.id === copyValue.value)
    }
    if (!isExit) {
      handleChange(undefined)
    }
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

  .all-tag {
    position: absolute;
    right: 5px;
    top: 50%;
    transform:translate(0,-50%);
    border: none;
    user-select: none;
  }

  ::v-deep(.el-input__inner) {
    padding-right: 50px;
  }
  ::v-deep(.el-input__suffix){
    right: 35px;
  }
}
</style>
