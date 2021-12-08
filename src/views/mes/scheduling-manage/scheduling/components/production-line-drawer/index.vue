<template>
  <common-drawer
    ref="drawerRef"
    title="选择需要分配的生产线"
    v-model="drawerVisible"
    direction="rtl"
    :before-close="handleClose"
    size="38.2%"
  >
    <template #titleRight>
      <common-button type="warning" size="mini" @click.stop="reset">重置生产线</common-button>
    </template>
    <template #content>
      <div class="tip">
        <span>* 注意：</span>
        <span> 在录入分配任务状态下，“取消”在此次分配中有修改的生产线，此次针对该生产线做得修改“不会还原” </span>
      </div>
      <div class="el-drawer-container" :style="{ 'max-height': `${maxHeight}px` }">
        <production-line-box :lines="list" @change="handleChange" />
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { defineProps, defineEmits, ref, watch, reactive } from 'vue'

import { deepClone } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import productionLineBox from '../production-line-box'

const drawerRef = ref()
const emit = defineEmits(['update:visible', 'changeLines'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  lines: {
    type: Array,
    default: () => []
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', closeHook: handleBeforeClose })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header', '.tip'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    extraHeight: 85,
    clientHRepMainH: true
  },
  drawerRef
)

const list = ref([])
const updateLines = reactive({})

watch(
  [() => props.visible, () => props.lines],
  ([visible]) => {
    if (visible) {
      init()
    }
  },
  { immediate: true }
)

function init() {
  // 深拷贝数组
  list.value = deepClone(props.lines)
}

function handleBeforeClose() {
  handleLines()
}

function handleLines() {
  // 在关闭界面时再给主页面的line赋值，避免由于主界面dom重绘过慢，导致当前界面选择效果的展示(dom重绘)也变慢
  const _updateIds = []
  const changeLines = {}
  for (const key in updateLines) {
    if (updateLines[key]) {
      _updateIds.push(+key)
    }
    updateLines[key] = false
  }
  for (const _workshop of props.lines) {
    const _productionLineList = _workshop.productionLineList || []
    for (const line of _productionLineList) {
      if (_updateIds.includes(line.id)) {
        line.selected = !line.selected
        changeLines[line.id] = line.selected
      }
    }
  }
  emit('changeLines', changeLines)
}

function handleChange({ workshop, line }) {
  if (line.sourceSelected === undefined) {
    line.sourceSelected = line.selected
  }
  line.selected = !line.selected
  updateLines[line.id] = line.selected !== line.sourceSelected
}

// 重置
function reset() {
  list.value.forEach((workshop) => {
    const productionLineList = workshop.productionLineList || []
    productionLineList.forEach((line) => {
      if (line.sourceSelected === undefined) {
        line.sourceSelected = line.selected
      }
      line.selected = false
      updateLines[line.id] = line.selected !== line.sourceSelected
    })
  })
}
</script>

<style lang="scss" scoped>
.tip {
  display: flex;
  flex-direction: row;
  justify-content: flex-start;
  align-items: flex-start;
  color: red;
  font-size: 13px;
  margin-bottom: 15px;
  line-height: 20px;
  > span {
    display: inline-block;
  }
  > span:nth-child(1) {
    width: 50px;
    flex-shrink: 0;
  }
}

.el-drawer-container {
  overflow: auto;
}
</style>
