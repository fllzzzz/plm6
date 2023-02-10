<template>
  <common-drawer
    ref="drawerRef"
    :visible="crud.detailVisible"
    :content-loading="!clsLoaded || crud.detailLoading"
    :before-close="crud.cancelDetail"
    :title="`红冲单：${detail?.supplementNo}`"
    :show-close="true"
    size="620px"
    custom-class="purchase-order-raw-mat-detail"
  >
    <template #content>
      <div class="main-content">
        <el-form v-if="isNotBlank(detail)" :model="detail" size="small" label-position="left" label-width="100px">
          <div class="form-left">
            <el-form-item label="创建时间" prop="createTime">
              <span v-parse-time="detail.createTime" />
            </el-form-item>

            <el-form-item label="退库单号" prop="receipt.serialNumber">
              <span v-empty-text>{{ detail.receipt?.serialNumber }}</span>
            </el-form-item>

            <el-form-item label="项目" prop="project">
              <span class="project-name">{{ projectNameFormatter(detail.project) }}</span>
            </el-form-item>

            <el-form-item label="单体" prop="monomerName">
              <span v-empty-text>{{ detail.monomerName }}</span>
            </el-form-item>

            <el-form-item label="区域" prop="areaName">
              <span v-empty-text>{{ detail.areaName }}</span>
            </el-form-item>

            <el-form-item label="车间" prop="workshop.name">
              <span v-empty-text>{{ detail.workshop?.name }}</span>
            </el-form-item>

            <el-form-item label="红冲金额" prop="amount">
              <span v-thousand="detail.amount" />
            </el-form-item>

            <el-form-item label="物料编号" prop="serialNumber">
              <span v-thousand="detail.serialNumber" />
            </el-form-item>

            <el-form-item label="物料种类" prop="basicClass">
              <span v-parse-enum="{ e: matClsEnum, v: detail.basicClass, bit: true, split: ' | ' }" />
            </el-form-item>

            <el-form-item label="规格" prop="specification">
              <span v-empty-text>{{ specFormat(detail) }}</span>
            </el-form-item>

              <el-form-item label="核算量" prop="mete">
              <span v-empty-text>{{ detail.mete }}</span>
            </el-form-item>

              <el-form-item label="核算单位" prop="accountingUnit">
              <span v-empty-text>{{ detail.accountingUnit }}</span>
            </el-form-item>

            <el-form-item label="品牌" prop="brand">
              <span v-empty-text>{{ detail.brand }}</span>
            </el-form-item>
          </div>
        </el-form>
      </div>
    </template>
  </common-drawer>
</template>

<script setup>
import { matClsEnum } from '@enum-ms/classification'
import { projectNameFormatter } from '@/utils/project'
import { isNotBlank } from '@/utils/data-type'
import { specFormat } from '@/utils/wms/spec-format'
import { setSpecInfoToList } from '@/utils/wms/spec'
import { numFmtByBasicClass } from '@/utils/wms/convert-unit'

import { regDetail } from '@compos/use-crud'
import useMatClsList from '@/composables/store/use-mat-class-list'

const { CRUD, crud, detail } = regDetail()

const { loaded: clsLoaded } = useMatClsList()

CRUD.HOOK.beforeDetailLoaded = async (crud, detail) => {
  // 红冲单号和物料种类字段冲突了
  detail.supplementNo = detail.serialNumber
  const list = [detail]
  await setSpecInfoToList(list)
  detail = await numFmtByBasicClass(list)[0]
}
</script>
