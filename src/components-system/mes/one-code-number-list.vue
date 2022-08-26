<template>
  <div class="tag-container" :style="tagContainerStyle">
    <template v-for="item in list" :key="item.number">
      <el-tag
        :type="isSelected(item) ? 'success' : 'info'"
        size="medium"
        :effect="isSelected(item) ? undefined : 'plain'"
        @click="handleSelectChange(item.number, item)"
        class="pointer"
      >
        <span>{{ item.number }}</span>
        <slot name="suffix" :data="item"></slot>
      </el-tag>
    </template>
  </div>
</template>

<script setup>
import { defineProps, defineEmits, ref, watch, computed } from 'vue'

const emit = defineEmits(['change', 'update:modelValue'])

const props = defineProps({
  modelValue: {
    type: [Array, Number, String]
  },
  list: {
    type: Array,
    default: () => []
  },
  tagWidth: {
    type: Number,
    default: 200
  },
  multiple: {
    type: Boolean,
    default: true
  },
  maxHeight: {
    type: Number
  }
})

const singleSelected = ref()
const selected = ref({})
const selectNum = computed(() => {
  return props.multiple ? Object.keys(selected.value) : singleSelected.value
})
const tagContainerStyle = computed(() => {
  return {
    'grid-template-columns': `repeat(auto-fill, ${props.tagWidth}px)`,
    'max-height': props.maxHeight ? `${props.maxHeight}px` : 'auto',
    overflow: 'auto'
  }
})

watch(
  () => props.modelValue,
  (val) => {
    if (props.multiple) {
      selected.value = getSelected(val)
    } else {
      singleSelected.value = val
    }
  },
  { immediate: true }
)

function isSelected(item) {
  return props.multiple ? selected.value[item.number] : Number(item.number) === singleSelected.value
}

function getSelected(selectedList) {
  const obj = {}
  for (let i = 0; i < selectedList.length; i++) {
    const item = props.list.find((v) => Number(v.number) === Number(selectedList[i]))
    obj[item.number] = item
  }
  return obj
}

function handleSelectChange(number, item) {
  if (props.multiple) {
    if (selected.value[number]) {
      delete selected.value[number]
    } else {
      selected.value[number] = item
    }
  } else {
    singleSelected.value = number
  }
  console.log(number, item)
  emit('update:modelValue', selectNum.value)
  emit('change', selectNum.value)
}
</script>

<style lang="scss" scoped>
.tag-container {
  display: grid;
  grid-row-gap: 10px;
  grid-column-gap: 10px;
  // grid-template-columns: 48% 48%;
  justify-content: stretch;

  .el-tag {
    width: 100%;
    user-select: none;
    text-align: center;
  }
}
.tag-container::-webkit-scrollbar {
  width: 0 !important;
}
</style>
