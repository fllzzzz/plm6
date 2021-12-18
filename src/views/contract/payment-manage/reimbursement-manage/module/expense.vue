<!-- 项目：部门人员列表 -->
<template>
  <el-cascader
    ref="expenseRef"
    class="expense-cascader"
    v-model="copyValue"
    :placeholder="placeholder"
    :options="options"
    :props="cascaderProps"
    :show-all-levels="showAllLevels"
    :separator="separator"
    :clearable="clearable"
    :disabled="disabled"
    filterable
    :size="size"
    @change="handleChange"
  />
</template>

<script setup>
import { defineExpose, defineProps, defineEmits, computed, watch, ref } from 'vue'
import { isNotBlank, isBlank, deepClone, judgeSameValue } from '@data-type/index'
import { get as getExpense } from '@/api/contract/expense-config'

const emit = defineEmits(['change', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: [Array, Number, String],
    default: '',
  },
  // 禁用值 id
  disabledVal: {
    type: Array,
    default: () => [],
  },
  // 大小
  size: {
    type: String,
    default: 'small',
  },
  // 是否可清除
  clearable: {
    type: Boolean,
    default: false,
  },
  // 是否禁用
  disabled: {
    type: Boolean,
    default: false,
  },
  // 输入框显示全路径
  showAllLevels: {
    type: Boolean,
    default: true,
  },
  // 分隔符
  separator: {
    type: String,
    default: '/',
  },
  // 提示
  placeholder: {
    type: String,
    default: '请选择',
  },
  // 多选
  multiple: {
    type: Boolean,
    default: false,
  },
  checkStrictly: {
    type: Boolean,
    default: true,
  },
  // 返回结果全路径
  emitPath: {
    type: Boolean,
    default: false,
  },
  // 额外的选项
  extraOption: {
    type: Object,
    require: false,
  },
  dataIndex: {
    type: Number,
  },
})

const expenseRef = ref()
const copyValue = ref()

const options = ref([])

const cascaderProps = computed(() => {
  return {
    value: 'id',
    label: 'label',
    children: 'links',
    expandTrigger: 'hover',
    emitPath: props.emitPath,
    multiple: props.multiple,
    checkStrictly: props.checkStrictly,
  }
})

getExpenseData()

async function getExpenseData() {
  let data = []
  try {
    const { content = [] } = await getExpense()
    if (content && content.length > 0) {
      content.forEach((v) => {
        v.label = v.name
        v.type = 1
        if (v.links.length > 0) {
          v.links.forEach((val) => {
            val.parentId = v.id
            val.type = 2
          })
        }
      })
    }
    data = content
  } catch (e) {
    console.log('获取报销类别', e)
  } finally {
    options.value = data.length > 0 ? data : []
  }
}

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

watch(
  () => props.disabledVal,
  () => {
    setNodeDisabled(options.value)
  },
  { immediate: true, deep: true }
)

// 发生change
function handleChange(val) {
  // 发生变化
  const isChange = !judgeSameValue(val, props.modelValue)
  // 两个值都为空
  const allBlank = isBlank(val) && isBlank(props.modelValue)

  if (isChange && !allBlank) {
    emit('update:modelValue', val)
    const choseVal = getNodeInfo()
    if (isNotBlank(choseVal)) {
      choseVal.dataIndex = props.dataIndex
    }
    emit('change', choseVal)
    return true
  }
  return false
}

// 设置禁用的节点
function setNodeDisabled(list) {
  list.forEach((v) => {
    v.disabled = props.disabledVal.includes(v.id)
    if (isNotBlank(v.links)) {
      setNodeDisabled(v.links)
    }
    return v
  })
}

// 单选 获取选中节点信息
function getNodeInfo() {
  const node = expenseRef.value.getCheckedNodes()
  return node.length ? node[0].data : {}
}

defineExpose({
  getNodeInfo,
})
</script>

<style lang="scss" scoped>
.expense-cascader {
  width: 220px;
}
</style>