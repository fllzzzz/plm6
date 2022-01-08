<template>
  <common-drawer
    ref="drawerRef"
    v-model="dialogVisible"
    :before-close="handleClose"
    title="退货匹配列表"
    size="70%"
    direction="btt"
    custom-class="reject-match-list-drawer"
  >
    <template #titleRight>
      <div class="statistical-info">
        <span>
          <span class="label">只看填写退货数量的材料</span>
          <el-checkbox v-model="filterParams.hasRejectNumber" size="mini"></el-checkbox>
        </span>
        <span>
          <span class="label">本次退货统计</span>
          <span class="color-green">{{ rejectTotalNumber }}</span> &nbsp;
          <span>{{ `/ ${material.rejectMaxNumber} ${material.outboundUnit}` }}</span>
        </span>
        <span>
          <span class="label">审核中的数量</span>
          <span>{{ `${material.rejectPendingNumber} ${material.outboundUnit}` }}</span>
        </span>
      </div>
    </template>
    <template #content>
      <div class="current-material flex-rss">
        <vertical-label name="当前物料" left="5px" />
        <common-table :data="[material]" row-key="id">
          <!-- 基础信息 -->
          <material-base-info-columns :basic-class="material.basicClass" :show-index="false" />
          <!-- 次要信息 -->
          <material-secondary-info-columns :basic-class="material.basicClass" />
          <!-- 单位及其数量 -->
          <material-unit-quantity-columns :basic-class="material.basicClass" />
          <!-- 仓库位置信息 -->
          <warehouse-info-columns show-project />
        </common-table>
      </div>
      <el-divider class="divider"><span class="title">匹配物料</span></el-divider>
      <div class="match-list-table">
        <common-table v-loading="matchListLoading" :data="filterMatchList" row-key="id" :max-height="maxHeight">
          <!-- 基础信息 -->
          <material-base-info-columns :basic-class="material.basicClass" show-frozen-tip frozen-viewable @refresh="handleRefresh" />
          <!-- 次要信息 -->
          <material-secondary-info-columns :basic-class="material.basicClass" />
          <!-- 单位及其数量 -->
          <material-unit-quantity-columns :basic-class="material.basicClass" outbound-type-mode />
          <!-- 仓库位置信息 -->
          <warehouse-info-columns show-project />
          <el-table-column label="退货数量" width="170px" align="center" fixed="right">
            <template #default="{ row }">
              <span class="flex-rbc">
                <common-input-number
                  v-model="row.rejectNumber"
                  :min="0"
                  :precision="row.outboundUnitPrecision"
                  :max="material.rejectMaxNumber > row.maxNumber ? row.maxNumber : material.rejectMaxNumber"
                  controls-position="right"
                  @change="(newVal, oldVal) => rejectNumberChange(row, newVal, oldVal)"
                />
                <span style="flex: none; margin-left: 10px">{{ row.outboundUnit }}</span>
              </span>
            </template>
          </el-table-column>
        </common-table>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { getMatchList } from '@/api/wms/reject/application'
import { defineEmits, defineProps, ref, watch, computed, nextTick } from 'vue'
import useVisible from '@/composables/use-visible'

import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'
import warehouseInfoColumns from '@/components-system/wms/table-columns/warehouse-info-columns/index.vue'
import verticalLabel from '@/components-system/common/vertical-label.vue'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { measureTypeEnum } from '@/utils/enum/modules/wms'
import { isNotBlank } from '@/utils/data-type'
import { ElMessage } from 'element-plus'
import useMaxHeight from '@/composables/use-max-height'

const emit = defineEmits(['change', 'update:visible'])

const props = defineProps({
  visible: {
    type: Boolean,
    require: true
  },
  basicClass: {
    type: Number
  },
  material: {
    type: Object,
    default: () => {
      return {}
    }
  },
  rejectInfo: {
    // 选中列表
    type: Object,
    default: () => {
      return {}
    }
  }
})

const filterParams = ref({
  hasRejectNumber: false
})

const rejectTotalNumber = ref(0)
const matchList = ref()
const matchListLoading = ref(false)
const rejectKV = ref({})

const { visible: dialogVisible, handleClose } = useVisible({ emit, props, field: 'visible' })

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.reject-match-list-drawer',
    extraBox: ['.el-drawer__header', '.current-material', '.divider'],
    wrapperBox: ['.el-drawer__body'],
    navbar: false,
    minHeight: 200,
    extraHeight: -20
  },
  dialogVisible
)

const filterMatchList = computed(() => {
  if (filterParams.value.hasRejectNumber) {
    return matchList.value.filter((row) => row.rejectNumber > 0)
  }
  return matchList.value
})

watch(
  [() => props.visible, () => props.material],
  ([nVisible, nMaterial]) => {
    if (nVisible) {
      init()
      if (nMaterial && nMaterial.id) {
        fetchMatchList(nMaterial.id)
      }
    }
  },
  { immediate: true }
)

function init() {
  rejectKV.value = props.rejectInfo
  // 计算当前物料本次退货统计
  rejectTotalNumber.value = 0
  Object.keys(rejectKV.value).forEach((id) => {
    rejectTotalNumber.value += rejectKV.value[id].rejectNumber
  })
  matchList.value = []
}

// 加载匹配列表
async function fetchMatchList(materialId) {
  try {
    matchListLoading.value = true
    const { content = [] } = await getMatchList(materialId)
    await setSpecInfoToList(content)
    await numFmtByBasicClass(content)
    matchList.value = content
    // 记录是否有数量应超过可退货数量而变化
    let numberHasChange = false
    // 列表中存在的退库信息
    const exitRejectIds = {}
    matchList.value.forEach((row) => {
      row.maxNumber = row.outboundUnitType === measureTypeEnum.MEASURE.V ? row.quantity : row.mete
      // 数据回填
      const rejectRow = rejectKV.value[row.id]
      if (rejectRow) {
        exitRejectIds[row.id] = true
        rejectKV.value[row.id] = row
        if (rejectRow.rejectNumber > row.maxNumber) {
          rejectNumberChange(undefined, row)
          numberHasChange = true
        } else {
          row.rejectNumber = rejectRow.rejectNumber
        }
      }
    })
    // 重新计算退货统计
    rejectTotalNumber.value = 0
    Object.keys(rejectKV.value).forEach((id) => {
      if (!exitRejectIds[id]) {
        delete rejectKV.value[id]
        numberHasChange = true
      } else {
        rejectTotalNumber.value += rejectKV.value[id].rejectNumber
      }
    })
    if (numberHasChange) {
      ElMessage.warning('清除了匹配列表中“可退货数量小于填写数量”的数量')
    }
  } catch (error) {
    console.error('加载匹配列表', error)
  } finally {
    matchListLoading.value = false
  }
}

// 退货数量
function rejectNumberChange(row, newVal, oldVal) {
  nextTick(() => {
    const number = rejectTotalNumber.value + (newVal || 0) - (oldVal || 0)
    if (number > props.material.rejectMaxNumber) {
      row.rejectNumber = oldVal
      ElMessage.warning('您填写的数量超过可退货数量，已为您取消本次操作')
      console.log('row', row)
      return
    }
    rejectTotalNumber.value = number
    const hasVal = isNotBlank(newVal) && newVal > 0
    // 在退货列表中，数量为0或不存在
    if (rejectKV.value[row.id] && !hasVal) {
      delete rejectKV.value[row.id]
    }
    // 不在退货列表中, 数量大于0且
    if (!rejectKV.value[row.id] && hasVal) {
      rejectKV.value[row.id] = row
    }
    emit('change')
  })
}

// 刷新
function handleRefresh() {
  fetchMatchList(props.material.id)
}
</script>

<style lang="scss" scoped>
.current-material {
  position: relative;
  margin-bottom: 30px;
}

.match-list-table {
  position: relative;
}

.statistical-info {
  display: flex;
  justify-content: flex-end;
  align-items: center;
  text-align: right;
  margin-right: 20px;
  > span:nth-child(n) {
    display: flex;
    align-items: center;
    font-size: 14px;
    line-height: 32px;
    margin-right: 30px;
    .label {
      font-weight: 700;
      padding-right: 12px;
      color: var(--el-text-color-regular);
    }
  }
  > span:last-child {
    margin-right: 0;
  }
}
</style>
