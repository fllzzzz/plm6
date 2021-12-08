<template>
  <common-drawer
    ref="drawerRef"
    title="快速分配"
    v-model="drawerVisible"
    direction="btt"
    :before-close="handleClose"
    :wrapper-closable="false"
    size="95%"
  >
    <template #titleRight>
      <common-button :disabled="!saveAble" type="primary" size="mini" @click="submit">预览并保存</common-button>
    </template>
    <template #content>
      <div class="el-drawer-container">
        <div class="table-content">
          <common-table
            ref="table"
            :max-height="maxHeight"
            :data="assignAbleList"
            empty-text="暂无数据"
            @selection-change="handleSelectionChange"
          >
            <el-table-column fixed type="selection" width="55" align="center" />
            <el-table-column fixed label="序号" type="index" align="center" width="60" />
            <!-- <el-table-column prop="projectName" :show-overflow-tooltip="true" label="项目" width="120px" />
            <el-table-column prop="monomerName" :show-overflow-tooltip="true" label="单体" width="120px" />
            <el-table-column prop="districtName" :show-overflow-tooltip="true" label="区域" width="120px" /> -->
            <template v-for="item in needTableColumns" :key="item.field">
              <el-table-column
                v-if="item.toFixed"
                fixed
                :show-overflow-tooltip="true"
                :prop="item.field"
                :label="item.label"
                align="center"
                :width="item.width"
              >
                <template v-slot="scope">
                  {{ toFixed(scope.row[item.field], item.DP) }}
                </template>
              </el-table-column>
              <el-table-column v-else fixed :show-overflow-tooltip="true" :prop="item.field" :label="item.label" :width="item.width" />
            </template>
            <el-table-column key="unassignQuantity" fixed="right" prop="unassignQuantity" label="未分配" align="center" min-width="70px">
              <template v-slot="scope">
                <span style="color: #13ce66">{{ scope.row.unassignQuantity }}</span>
              </template>
            </el-table-column>
            <el-table-column key="assignQuantity" fixed="right" prop="assignQuantity" label="已分配" align="center" min-width="70px">
              <template v-slot="scope">
                <span style="color: #e6a700">{{ scope.row.assignQuantity }}</span>
              </template>
            </el-table-column>
            <el-table-column key="quantity" fixed="right" prop="quantity" label="数量" align="center" min-width="70px" />
          </common-table>
        </div>
        <div class="line-content" :style="{ 'max-height': `${maxHeight}px` }">
          <div class="tip">
            <span>* 注意：</span>
            <div>
              <span style="display: block">1. 可以快速分配的构件：构件未被禁止操作，构件任务未暂停，构件未处于报废或二次利用状态。</span>
              <span style="display: block">2. 快速分配只能选择一条生产线，所勾选构件的未分配数量会全部分配给该生产线。</span>
            </div>
          </div>
          <production-line-box :lines="lines" @change="handleChange" :selectLineId="selectLineId" isSingle />
        </div>
      </div>
    </template>
  </common-drawer>
  <mPreview v-model:visible="previewVisible" :data="submitData" :lines="selectLine" @success="handleSaveSuccess" />
</template>

<script setup>
import { computed, defineProps, defineEmits, ref, watch, inject } from 'vue'

import { toFixed } from '@data-type'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import productionLineBox from '../production-line-box'
import mPreview from '../scheduling-preview'

const drawerRef = ref()
const emit = defineEmits(['update:visible', 'success'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  data: {
    type: Array,
    default: () => []
  },
  lines: {
    type: Array,
    default: () => []
  }
})

const needTableColumns = inject('needTableColumns')
const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

// 高度
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.common-drawer',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body', '.el-drawer-container'],
    navbar: false,
    extraHeight: 130,
    clientHRepMainH: true
  },
  drawerRef
)
const selectLineId = ref()
const previewVisible = ref(false)
const multipleSelection = ref([])
const assignAbleList = ref([]) // 可分配列表
const selectLine = ref([])
const submitData = ref([])

const saveAble = computed(() => multipleSelection.value && multipleSelection.value.length > 0 && selectLineId.value)

watch(
  [() => props.visible, () => props.lines],
  ([visible]) => {
    if (visible) {
      handleDataChange()
    }
  },
  { immediate: true }
)

function submit() {
  handleSubmitData()
  previewVisible.value = true
}

function handleChange({ line }) {
  selectLineId.value = line.id
}

function handleDataChange() {
  // 处理在录入的情况下打开快速分配
  let _data = JSON.parse(JSON.stringify(props.data)) || []
  _data = _data.filter((v) => {
    return v.unassignQuantity
  })
  assignAbleList.value = _data.map((v) => {
    v.schedulingMap = JSON.parse(JSON.stringify(v.sourceSchedulingMap))
    v.assignQuantity = v.sourceAssignQuantity // 已分配数量
    v.unassignQuantity = v.sourceUnassignQuantity // 未分配数量还原
    return v
  })
  console.log(assignAbleList, 'assignAbleList')
}

function handleSubmitData() {
  const _lines = JSON.parse(JSON.stringify(props.lines))
  console.log(_lines)
  selectLine.value = _lines.filter((f) => {
    // 过滤一遍生产线，以便加快预览界面数据处理
    f.productionLineList = f.productionLineList.filter((l) => l.id === selectLineId.value)
    return f.productionLineList && f.productionLineList.length > 0
  })
  console.log(multipleSelection.value, 'multipleSelection.value')
  submitData.value = JSON.parse(JSON.stringify(multipleSelection.value))
  submitData.value.forEach((v) => {
    // 处理任务数据，修改任务数量及未分配任务数量
    console.log(v, 'v')
    v.schedulingMap[selectLineId.value].quantity = v.schedulingMap[selectLineId.value].quantity || 0
    v.schedulingMap[selectLineId.value].quantity += v.sourceUnassignQuantity
    v.assignQuantity += v.sourceUnassignQuantity
    v.unassignQuantity = 0
  })
}

function handleSelectionChange(val) {
  multipleSelection.value = val
}

function handleSaveSuccess() {
  emit('success')
  handleClose()
}
</script>

<style lang="scss" scoped>
.el-drawer-container {
  box-sizing: border-box;
  display: flex;
  flex-direction: row;
  justify-content: center;
  align-items: flex-start;
}
.el-drawer-container .table-content {
  flex-grow: 1;
  width: 70%;
}
.line-content {
  padding: 14px 20px 20px 40px;
  box-sizing: border-box;
  overflow: auto;
  flex-grow: 1;
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
}
</style>
