<!-- 普通按钮, 避免使用el-button 时产生, disabled后仍可通过点击按钮中的文字触发click事件的BUG -->
<!-- 部分功能需要捕获click（列：el-upload上传）, 因此在未禁用（disabled）的情况下不使用.stop修饰符 -->
<template>
  <el-button
    class="common-button"
    v-if="slotDefault && props.disabled"
    v-bind="$attrs"
    :icon="icon"
    :size="props.size"
    :type="props.type"
    :disabled="props.disabled"
    :loading="loading"
    :auto-insert-space="autoInsertSpace"
    @click="handleClick"
  >
    <span @click.stop="handleClick"><slot /></span>
  </el-button>
  <el-button
    v-else-if="!slotDefault && props.disabled"
    class="common-button"
    v-bind="$attrs"
    :icon="icon"
    :size="props.size"
    :type="props.type"
    :disabled="props.disabled"
    :loading="loading"
    :auto-insert-space="autoInsertSpace"
    @click.stop="handleClick"
  />
  <el-button
    v-else-if="!props.disabled && slotDefault"
    class="common-button"
    v-bind="$attrs"
    :icon="icon"
    :size="props.size"
    :type="props.type"
    :disabled="props.disabled"
    :loading="loading"
    :auto-insert-space="autoInsertSpace"
    @click="handleClick"
  >
    <slot />
  </el-button>
  <el-button
    v-else-if="!props.disabled && !slotDefault"
    class="common-button"
    v-bind="$attrs"
    :icon="icon"
    :size="props.size"
    :type="props.type"
    :disabled="props.disabled"
    :loading="loading"
    :auto-insert-space="autoInsertSpace"
    @click="handleClick"
  />
</template>

<script setup>
import { isBlank } from '@/utils/data-type'
import { ElButton } from 'element-plus'
import { defineProps, defineEmits, useSlots, ref } from 'vue'

// 判断<slot/>是否有传值,<el-button></el-button>会产生<span></span>,由与element-ui全局样式的影响, 当按钮只有图标时，这种情况会产生一个margin
const slotDefault = ref(!!useSlots().default)
if (slotDefault.value) {
  const slot = useSlots().default()
  // 避免slot为v-if=false的情况，出现span，从而导致图标未居中
  slotDefault.value = slot.some((v) => {
    // shapeFlag:某种类型,暂不知。 打包前v-if为false 显示 v.children为v-if 打包后为 “空”
    console.log('v.children', v, v.children)
    const blankFlag = v.shapeFlag === 8 && (isBlank(v.children) || v.children === 'v-if' || v.children === 'v-show')
    return !blankFlag
  })
}

const emit = defineEmits(['click'])

const props = defineProps({
  disabled: {
    // 提交禁用
    type: Boolean,
    default: false
  },
  loading: {
    // loading
    type: Boolean,
    default: false
  },
  size: {
    // 按钮大小
    type: String,
    default: 'small'
  },
  type: {
    // 按钮样式
    type: String
  },
  icon: {
    type: [String, Object]
  },
  autoInsertSpace: {
    // 自动在两个中文字符之间插入空格
    type: Boolean
  }
})

// 处理禁用，点击按钮内的文字仍可触发点击的情况
function handleClick(event) {
  if (props.disabled === true) return
  emit('click', event)
}
</script>
