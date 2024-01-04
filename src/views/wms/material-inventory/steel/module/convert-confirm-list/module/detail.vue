<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.detailVisible"
    :content-loading="crud.detailLoading"
    :before-close="crud.cancelDetail"
    :title="'详情'"
    :show-close="true"
    size="85%"
    custom-class="convert-detail"
  >
    <template #titleAfter>
      <el-tag type="success">条板转换单：{{detail.receiptSerialNumber}}</el-tag>
      <el-tag type="info" v-if="detail.status===reviewStatusEnum.PASS.V">出库单：{{detail.outSerialNumber}}</el-tag>
      <el-tag type="danger" v-if="detail.boolPartyA">甲供</el-tag>
    </template>
    <template #content>
      <el-tag effect="plain">原物料信息</el-tag>
      <el-descriptions :column="3" border style="margin:10px 0;">
        <el-descriptions-item label-class-name="contractLabel" label="物料名称">{{ detail.classifyName || '-' }}</el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="物料编号">{{ detail.serialNumber || '-' }}</el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="规格">
          {{ detail.specification || '-' }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="厚(mm)*宽(mm)">
          {{ detail.widthThick || '-' }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="品牌">
          {{ detail.brand || '-' }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="颜色">
          {{ detail.color || '-' }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="卷号">
          {{ detail.heatNoAndBatchNo || '-' }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="项目">
          {{ detail.project? projectNameFormatter(detail.project) : '-' }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="仓库">
          {{ detail.warehouse?.name || '-' }}
        </el-descriptions-item>
        <el-descriptions-item label-class-name="contractLabel" label="备注">
          {{ detail.remark || '-' }}
        </el-descriptions-item>
      </el-descriptions>
      <el-divider><span class="title">转换后物料信息</span></el-divider>
      <common-table
        :data="detail.convertDetailDTOList"
        v-loading="loading"
        :data-format="dataFormat"
        show-summary
        :summary-method="getSummaries"
        :max-height="maxHeight-220"
      >
        <el-table-column label="序号" type="index" align="center" width="60">
          <template #default="{ row: { sourceRow: row },$index }">
            <span v-if="row.boolSurplus">余料</span>
            <span v-else>{{$index+1}}</span>
          </template>
        </el-table-column>
        <el-table-column key="serialNumber" prop="serialNumber" label="物料编号" align="center" show-overflow-tooltip />
        <el-table-column key="classifyName" prop="classifyName" label="物料种类" align="center" show-overflow-tooltip />
        <el-table-column key="length" prop="length" label="长(mm)" align="center" show-overflow-tooltip />
        <el-table-column key="width" prop="width" label="宽(mm)" align="center" show-overflow-tooltip />
        <el-table-column key="thickness" prop="thickness" label="厚(mm)" align="center" show-overflow-tooltip />
        <el-table-column key="specification" prop="specification" label="规格" align="center" show-overflow-tooltip />
        <el-table-column key="quantity" prop="quantity" label="数量（张）" align="center" show-overflow-tooltip />
        <el-table-column key="mete" prop="mete" label="重量（kg）" align="center" show-overflow-tooltip />
        <el-table-column key="project" prop="project" label="所属项目" align="center" min-width="150" show-overflow-tooltip />
        <el-table-column key="monomerName" prop="monomerName" label="单体" align="center" show-overflow-tooltip />
        <el-table-column key="areaName" prop="areaName" label="区域" align="center" show-overflow-tooltip />
        <el-table-column key="workshopName" prop="workshopName" label="车间" align="center" show-overflow-tooltip />
        <el-table-column key="warehouse.name" prop="warehouse.name" label="仓库" align="center" show-overflow-tooltip />
        <el-table-column key="recipientName" prop="recipientName" label="领用人" align="center" show-overflow-tooltip />
        <el-table-column key="outboundTime" prop="outboundTime" label="出库时间" align="center" show-overflow-tooltip width="150" />
        <el-table-column key="boolOutbound" prop="boolOutbound" label="是否出库" align="center" show-overflow-tooltip />
      </common-table>
    </template>
  </common-drawer>
</template>

<script setup>
import { ref } from 'vue'
import { tableSummary } from '@/utils/el-extra'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { projectNameFormatter } from '@/utils/project'

import { whetherEnum, reviewStatusEnum } from '@enum-ms/common'

import { regDetail } from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'

const drawerRef = ref()

const dataFormat = ref([
  ['project', 'parse-project'],
  ['quantity', ['to-fixed-field', 'measurePrecision']],
  ['mete', ['to-fixed-field', 'accountingPrecision']],
  ['outboundTime', ['parse-time', '{y}-{m}-{d} {h}:{i}']],
  ['boolOutbound', ['parse-enum', whetherEnum]]
])
const { CRUD, crud, detail } = regDetail()

const { maxHeight } = useMaxHeight(
  {
    mainBox: '.convert-detail',
    extraBox: '.el-drawer__header',
    wrapperBox: '.el-drawer__body',
    paginate: true,
    minHeight: 300,
    navbar: false,
    clientHRepMainH: true
  },
  drawerRef
)

CRUD.HOOK.beforeDetailLoaded = async (crud, detail) => {
  detail.widthThick = (detail.thickness || detail.width) ? (detail.thickness || '-') + '*' + (detail.width || '-') : '-'
  await setSpecInfoToList(detail.convertDetailDTOList)
  detail.convertDetailDTOList = await numFmtByBasicClass(
    detail.convertDetailDTOList,
    {
      toSmallest: false,
      toNum: true
    }
  )
}

// 合计
function getSummaries(param) {
  return tableSummary(param, { props: ['quantity', 'mete'] })
}
</script>

<style lang="scss" scoped>
.raw-mat-inbound-application-record-detail {
  .el-drawer__header .el-tag {
    min-width: 70px;
    text-align: center;
  }
  .el-table {
    ::v-deep(td .cell) {
      min-height: 28px;
      line-height: 28px;
    }
  }
}
</style>
