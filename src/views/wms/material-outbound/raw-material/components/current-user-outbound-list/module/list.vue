<template>
  <common-drawer
    ref="drawerRef"
    v-model="drawerVisible"
    :content-loading="detailLoading"
    :before-close="handleClose"
    title="出库清单"
    :show-close="true"
    size="100%"
    custom-class="raw-mat-application-review-form"
  >
    <template v-if="checkPermission(permission.outbound)" #titleRight>
      <template v-if="form.reviewStatus === reviewStatusEnum.UNREVIEWED.V && isNotBlank(form.list)">
        <span class="batch-set-info child-mr-7">
          <el-date-picker v-model="batchOutboundTime" type="datetime" value-format="x" placeholder="批量设置出库时间" />
          <common-button type="success" size="mini" @click="setOutboundTime">设置</common-button>
        </span>
        <el-popconfirm
          confirm-button-text="确定"
          cancel-button-text="取消"
          icon="el-icon-info"
          icon-color="green"
          title="确认通过？"
          @confirm="passed"
        >
          <template #reference>
            <common-button :loading="submitOptLoading" type="primary" size="mini" icon="el-icon-s-promotion"> 确认出库 </common-button>
          </template>
        </el-popconfirm>
        <el-popconfirm
          confirm-button-text="确定"
          cancel-button-text="取消"
          icon="el-icon-info"
          icon-color="red"
          title="确认清空？"
          @confirm="returned"
        >
          <template #reference>
            <common-button :loading="submitOptLoading" size="mini" icon="el-icon-document-delete" type="danger"> 清 空 </common-button>
          </template>
        </el-popconfirm>
      </template>
      <template v-if="form.reviewStatus === reviewStatusEnum.PASS.V">
        <print-table api-key="wmsRmOutboundReceipt" :params="form.id" size="mini" type="warning" class="filter-item"/>
      </template>
    </template>
    <template #content>
      <el-form class="form" :disabled="formDisabled">
        <common-table
          :data="form.list"
          :data-format="columnsDataFormat"
          :max-height="maxHeight"
          show-summary
          :summary-method="getSummaries"
          :expand-row-keys="expandRowKeys"
          row-key="id"
        >
          <el-expand-table-column :data="form.list" v-model:expand-row-keys="expandRowKeys" row-key="id" fixed="left">
            <template #default="{ row }">
              <expand-secondary-info :basic-class="row.basicClass" :row="row" show-remark show-graphics>
                <p v-if="row.boolTransfer">
                  调拨：
                  <span>（来源）</span>
                  <span style="color: brown">{{ row.sourceProject }}</span>
                  <span> ▶ </span>
                  <span>（目的）</span>
                  <span style="color: #3a8ee6">{{ row.project }}</span>
                </p>
              </expand-secondary-info>
            </template>
          </el-expand-table-column>
          <!-- 基础信息 -->
          <material-base-info-columns :basic-class="form.basicClass" show-outbound-mode fixed="left" />
          <!-- 次要信息 -->
          <material-secondary-info-columns />
          <!-- 单位及其数量 -->
          <material-unit-quantity-columns />
          <!-- 仓库信息 -->
          <warehouse-info-columns show-project show-transfer />
          <el-table-column label="领用人" width="100px" align="center">
            <template #default="{ row }">
              <el-tooltip v-if="row.recipient" placement="top" effect="light" :content="`${row.recipient.deptName}`">
                <span>{{ row.recipient.name }}</span>
              </el-tooltip>
            </template>
          </el-table-column>
          <el-table-column label="出库时间" width="195px" align="center">
            <template #default="{ row }">
              <el-date-picker
                v-if="form.reviewStatus === reviewStatusEnum.UNREVIEWED.V"
                v-model="row.sourceRow.outboundTime"
                type="datetime"
                value-format="x"
                placeholder="出库时间"
              />
              <span v-else>{{ row.outboundTime }}</span>
            </template>
          </el-table-column>
          <el-table-column
            v-if="checkPermission(permission.outbound) && form.reviewStatus === reviewStatusEnum.UNREVIEWED.V"
            label="操作"
            width="70px"
            align="left"
          >
            <template #default="{ row, $index }">
              <el-popconfirm
                confirm-button-text="确定"
                cancel-button-text="取消"
                icon="el-icon-info"
                icon-color="red"
                :title="`确认删除当前${row.classifyFullName}吗?`"
                @confirm="delItem(row, $index)"
              >
                <template #reference>
                  <common-button :loading="submitOptLoading" type="danger" icon="el-icon-delete" size="mini" />
                </template>
              </el-popconfirm>
            </template>
          </el-table-column>
        </common-table>
      </el-form>
    </template>
  </common-drawer>
</template>

<script setup>
import { getDetailByCurrentUser, reviewPassed, reviewReturned, delMaterial } from '@/api/wms/material-outbound/raw-material/review'
import { inject, computed, ref, defineEmits, defineProps } from 'vue'
import { reviewStatusEnum } from '@/utils/enum/modules/common'
import { tableSummary } from '@/utils/el-extra'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { isNotBlank } from '@/utils/data-type'
import { materialColumns } from '@/utils/columns-format/wms'
import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useVisible from '@compos/use-visible'
import elExpandTableColumn from '@comp-common/el-expand-table-column.vue'
import expandSecondaryInfo from '@/components-system/wms/table-columns/expand-secondary-info/index.vue'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import WarehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import CommonButton from '@/components-system/common/common-button/index.vue'

const emit = defineEmits(['refresh', 'update:visible'])

const props = defineProps({
  visible: {
    type: Boolean,
    default: false
  }
})

const permission = inject('permission')
const drawerRef = ref() // 当前drawer

const submitOptLoading = ref(false) // 操作loading
const detailLoading = ref(false) // 通过loading

const expandRowKeys = ref([]) // 展开
const form = ref({})
const operateRecordNumber = ref(0) // 操作记录数，大于0代表有操作，回到物料仓时需要刷新页面
const batchOutboundTime = ref() // 批量设置出库时间

// 表单禁止操作
const formDisabled = computed(() => submitOptLoading.value)

// 表格列格式化
const columnsDataFormat = ref([
  ...materialColumns,
  ['remark', 'empty-text'],
  ['project', 'parse-project'],
  ['sourceProject', 'parse-project'],
  ['outboundTime', ['parse-time', '{y}-{m}-{d} {h}:{i}:{s}']]
])
// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.raw-mat-application-review-form',
    extraBox: ['.el-drawer__header'],
    wrapperBox: ['.el-drawer__body'],
    clientHRepMainH: true,
    minHeight: 300
  },
  () => computed(() => !detailLoading.value)
)

const { visible: drawerVisible, handleClose } = useVisible({ emit, props, field: 'visible', showHook: showHook, closeHook: closeHook })

// 显示狗子
function showHook() {
  init()
  fetchDetail()
}

// 关闭钩子
function closeHook() {
  // 审核数量>0 则刷新
  if (operateRecordNumber.value) {
    emit('refresh')
  }
}

// 初始化
function init() {
  detailLoading.value = false // 详情loading
  submitOptLoading.value = false // 退回loading
  submitOptLoading.value = false // 通过loading
  expandRowKeys.value = [] // 展开
  form.value = {} // 表单重置
}

// 加载详情
async function fetchDetail() {
  try {
    detailLoading.value = true
    const data = await getDetailByCurrentUser()
    if (data) {
      form.value = await detailFormat(data)
    }
  } catch (error) {
    console.log('加载失败', error)
  } finally {
    detailLoading.value = false
  }
}

// 详情格式转换
async function detailFormat(form) {
  await setSpecInfoToList(form.list)
  form.list = await numFmtByBasicClass(form.list, {
    toSmallest: false,
    toNum: false
  })
  return form
}

// 删除出库单中的物料，删除也需要更新页面，避免材料类型发生变化，或者，当前出库单已不存在物料被自动删除
async function delItem(row, index) {
  try {
    submitOptLoading.value = true
    await delMaterial({ listId: form.value.id, materialId: row.id })
    form.value.list.splice(index, 1)
    ++operateRecordNumber.value
    if (form.value.list.length === 0) {
      init()
    }
  } catch (error) {
    console.log('删除', error)
  } finally {
    submitOptLoading.value = false
  }
}

// 通过提交
async function passed() {
  try {
    submitOptLoading.value = true
    await reviewPassed(form.value)
    // 审核通过后设置审核状态
    form.value.reviewStatus = reviewStatusEnum.PASS.V
    ++operateRecordNumber.value
  } catch (error) {
    console.log('通过提交', error)
  } finally {
    submitOptLoading.value = false
  }
}

// 退回
async function returned() {
  try {
    submitOptLoading.value = true
    await reviewReturned(form.value.id)
    ++operateRecordNumber.value
    init()
  } catch (error) {
    console.log('退回', error)
  } finally {
    submitOptLoading.value = false
  }
}

// 合计
function getSummaries(param) {
  return tableSummary(param, { props: ['quantity', 'mete'] })
}

// 设置出库时间
function setOutboundTime() {
  form.value.list.forEach((v) => {
    v.outboundTime = batchOutboundTime.value
  })
}
</script>

<style lang="scss" scoped>
.raw-mat-application-review-form {
  .el-drawer__header .el-tag {
    min-width: 120px;
    text-align: center;
  }

  .convenient-operation {
    margin-right: 10px;
  }
  .el-table {
    ::v-deep(td .cell) {
      min-height: 28px;
      line-height: 28px;
    }

    ::v-deep(.el-input--suffix .el-input__inner) {
      padding-right: 10px;
    }
  }
  .batch-set-info {
    padding-right: 10px;
    margin-right: 10px;
    border-right: 1px solid#dcdfe6;
  }
}
</style>
