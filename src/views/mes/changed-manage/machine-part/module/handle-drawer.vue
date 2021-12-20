<template>
  <common-drawer ref="drawerRef" :title="`${info.serialNumber}零件处理列表`" v-model="drawerVisible" direction="rtl" :before-close="handleClose" size="40%">
    <template #content>
      <div class="tip">
        <span>* 注意：</span>
        <span> 可修改的零件数量总和为{{ canHandleTotalMete }}，请谨慎操作！</span>
      </div>
      <common-table ref="tableRef" v-loading="tableLoading" :data="canHandleList" :max-height="maxHeight" style="width: 100%">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column prop="productionLine.name" :show-overflow-tooltip="true" label="生产线" min-width="150px">
          <template v-slot="scope">
            <span>{{ scope.row.productionLine?.name }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="taskQuantity" :show-overflow-tooltip="true" label="任务数" align="center" min-width="150px">
          <template v-slot="scope">
            <div v-if="scope.row.isEdit">
              <el-input-number
                class="align-left"
                v-model="scope.row.quantity"
                placeholder="请输入任务数"
                controls-position="right"
                style="width: 100%"
                size="mini"
                :min="scope.row.taskQuantity - Math.min(canHandleTotalMete, scope.row.taskQuantity - scope.row.completeQuantity)"
                :max="scope.row.taskQuantity"
              />
            </div>
            <span v-else>{{ scope.row.taskQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column prop="completeQuantity" :show-overflow-tooltip="true" label="已生产" align="center" width="100px">
          <template v-slot="scope">
            <span>{{ scope.row.completeQuantity }}</span>
          </template>
        </el-table-column>
        <el-table-column label="操作" width="170">
          <template v-slot="scope">
            <template v-if="scope.row.isEdit">
              <common-button
                :loading="submitLoading"
                :disabled="scope.row.taskQuantity === scope.row.quantity"
                size="mini"
                type="success"
                @click="submit(scope.row)"
              >
                确认
              </common-button>
              <common-button size="mini" type="warning" @click="cancelIt(scope.row)"> 取消 </common-button>
            </template>
            <common-button
              v-else
              type="primary"
              size="mini"
              :disabled="canHandleTotalMete <= 0 || scope.row.taskQuantity - scope.row.completeQuantity <= 0 || hasEdit"
              @click="editIt(scope.row)"
              >修改</common-button
            >
          </template>
        </el-table-column>
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { taskList } from '@/api/mes/changed-manage/common'
import { change } from '@/api/mes/changed-manage/machine-part'
import { defineProps, defineEmits, ref, watch } from 'vue'
import { ElNotification } from 'element-plus'

import { deepClone } from '@data-type/index'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'

const tableRef = ref()
const drawerRef = ref()
const emit = defineEmits(['update:visible', 'refresh'])
const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  },
  info: {
    type: Object,
    default: () => {}
  }
})

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

// 高度
const { maxHeight } = useMaxHeight(
  {
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)
const tableLoading = ref(false)
const submitLoading = ref(false)
const hasEdit = ref(false)
const canHandleList = ref([])
const canHandleTotalMete = ref()

watch(
  () => props.visible,
  (visible) => {
    if (visible) {
      fetchList()
      canHandleTotalMete.value = props.info.canHandleTotalMete
    } else {
      init()
    }
  },
  { immediate: true }
)

function init() {
  canHandleList.value = []
  if (canHandleTotalMete.value !== props.info.canHandleTotalMete) {
    emit('refresh')
  }
}

async function fetchList() {
  let _list = []
  try {
    tableLoading.value = true
    const { content } = await taskList({
      productType: props.info?.productType,
      productId: props.info?.productId
    })
    _list = content.map((v) => {
      v.submitLoading = false
      v.isEdit = false
      return v
    })
  } catch (error) {
    console.log('获取处理列表失败', error)
  } finally {
    canHandleList.value = deepClone(_list)
    tableLoading.value = false
  }
}

function editIt(row) {
  row.isEdit = true
  row.quantity = row.taskQuantity
  hasEdit.value = true
}

function cancelIt(row) {
  row.isEdit = false
  hasEdit.value = false
}

async function submit(row) {
  try {
    submitLoading.value = true
    await change({
      abnormalId: row.id,
      quantity: row.quantity
    })
    ElNotification({
      title: '零件变更处理成功',
      type: 'success',
      duration: 2500
    })
    row.isEdit = false
    hasEdit.value = false
    canHandleTotalMete.value = canHandleTotalMete.value - (row.taskQuantity - row.quantity)
    row.taskQuantity = row.quantity
  } catch (error) {
    console.log('变更处理失败', error)
  } finally {
    submitLoading.value = false
  }
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

.list-content {
  display: flex;
  flex-direction: row;
  justify-content: space-between;
  align-items: flex-start;

  .handle-content {
    box-sizing: border-box;
    padding-right: 20px;
    flex: auto;
    min-width: 260px;
    width: calc(100% - 980px);
  }
  .preview-content {
    flex: none;
    width: 980px;
  }
}

.handle-title {
  padding: 5px 0px;
  box-sizing: border-box;
  font-weight: bold;
  color: #303133;
  display: flex;
  justify-content: space-between;
  align-items: center;
}
</style>
