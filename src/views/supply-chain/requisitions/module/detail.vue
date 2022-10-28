<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.detailVisible"
    :content-loading="crud.detailLoading"
    :before-close="crud.cancelDetail"
    title="申购详情"
    :show-close="true"
    size="80%"
    custom-class="requisitions-application-record-detail"
  >
    <template #titleRight>
      <div class="print-wrap">
        <print-table
          v-permission="permission?.print"
          api-key="scmRequisitionsDetail"
          :params="{ id: detail?.id }"
          size="mini"
          type="warning"
          />
      </div>
    </template>
    <template #content>
      <div class="table-header">
        <el-tag effect="plain" size="medium">申购编号：{{ detail?.serialNumber }}</el-tag>
        <el-tag effect="plain" size="medium">备料类型：{{ preparationTypeEnum.VL?.[detail?.type] }}</el-tag>
        <el-tag type="success" effect="plain" size="medium">申购人：{{ detail?.applicantName }}</el-tag>
        <el-tag type="success" effect="plain" size="medium">到厂日期：{{ parseTime(detail?.arrivalTime, '{y}-{m}-{d}') }}</el-tag>
      </div>
      <common-table
        :data="detail.detailList"
        :data-format="columnsDataFormat"
        :max-height="maxHeight"
        show-summary
        :summary-method="getSummaries"
      >
        <!-- 基础信息 -->
        <material-base-info-columns :basic-class="detail.basicClass" fixed="left" />
        <!-- 单位及其数量 -->
        <material-unit-quantity-columns :basic-class="detail.basicClass" />
        <!-- 次要信息 -->
        <material-secondary-info-columns :basic-class="detail.basicClass" />
      </common-table>
      <div class="table-remark">
        <span>项目</span>
        <span>
          <span>{{ projectName }}</span>
        </span>
      </div>
      <div class="table-remark">
        <span>备注</span>
        <span>
          <span>{{ detail.remark }}</span>
        </span>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { inject, computed, ref } from 'vue'
import { tableSummary } from '@/utils/el-extra'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { materialColumns } from '@/utils/columns-format/wms'
import { matClsEnum } from '@/utils/enum/modules/classification'
import { parseTime } from '@/utils/date'
import { preparationTypeEnum } from '@enum-ms/wms'

import { regDetail } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import materialBaseInfoColumns from '@/components-system/wms/table-columns/material-base-info-columns/index.vue'
import materialUnitQuantityColumns from '@/components-system/wms/table-columns/material-unit-quantity-columns/index.vue'
import materialSecondaryInfoColumns from '@/components-system/wms/table-columns/material-secondary-info-columns/index.vue'

const permission = inject('permission')
// 表格列数据格式转换
const columnsDataFormat = ref([...materialColumns])

const drawerRef = ref()
const { CRUD, crud, detail } = regDetail()

// 表格高度处理
const { maxHeight } = useMaxHeight(
  {
    mainBox: '.requisitions-application-record-detail',
    extraBox: ['.el-drawer__header', '.table-header', '.table-remark'],
    wrapperBox: ['.el-drawer__body'],
    clientHRepMainH: true,
    minHeight: 300,
    extraHeight: 10
  },
  () => computed(() => !crud.detailLoading)
)

// 项目名称
const projectName = computed(() => {
  if (detail.type === preparationTypeEnum.PROJECT.V) {
    return detail.projects?.map(v => `${v.serialNumber} ${v.shortName}`)?.join('、')
  }
  return ''
})

CRUD.HOOK.beforeDetailLoaded = async (crud, detail) => {
  await setSpecInfoToList(detail.detailList)
  detail.detailList.forEach(v => {
    // 钢卷按米显示
    if (v.basicClass === matClsEnum.STEEL_COIL.V) {
      v.measureUnit = '米'
      v.measurePrecision = 3
    }
  })
  detail.detailList = await numFmtByBasicClass(detail.detailList, {
    toSmallest: false,
    toNum: false
  })
}

// 合计
function getSummaries(param) {
  return tableSummary(param, {
    props: [['quantity', 3], 'mete']
  })
}
</script>

<style lang="scss" scoped>
.requisitions-application-record-detail {
  .el-table {
    ::v-deep(td .cell) {
      min-height: 28px;
      line-height: 28px;
    }
  }
  .table-header {
    margin-bottom: 10px;
    > span + span {
      margin-left: 6px;
    }
  }
  .table-remark {
    height: 45px;
    display: flex;
    border: 1px solid #ebeef5;
    border-top-width: 0;
    font-size: 12px;
    color: #606266;
    >span:first-child {
      width: 55px;
      line-height: 44px;
      text-align: center;
      border-right: 1px solid #ebeef5;
    }
    >span:last-child {
      flex: 1;
      height: 40px;
      padding: 6px 10px;
      display: -webkit-box;
      overflow: hidden;
      text-overflow: ellipsis;
      -webkit-line-clamp: 2;
      -webkit-box-orient: vertical;
    }
  }
}
</style>
