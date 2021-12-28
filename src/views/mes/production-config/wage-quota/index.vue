<template>
  <div class="app-container">
    <!--工具栏-->
    <div class="head-container">
      <mHeader />
    </div>
    <!--表格渲染-->
    <common-table
      ref="tableRef"
      v-loading="crud.loading"
      :data="crud.data"
      :empty-text="crud.emptyText"
      :max-height="maxHeight"
      style="width: 100%"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('name') && crud.query.sequenceType !== typeEnum.MACHINE_PART.V"
        key="name"
        prop="name"
        :show-overflow-tooltip="true"
        label="名称"
        width="140px"
      />
      <el-table-column
        v-if="columns.visible('processType') && crud.query.sequenceType === typeEnum.ARTIFACT.V"
        key="processType"
        prop="processType"
        label="工序次序"
        width="100px"
      >
        <template v-slot="scope">
          <span>{{ processTypeEnum.VL[scope.row.processType] }}</span>
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('processSequence')"
        key="processSequence"
        prop="processSequence"
        :show-overflow-tooltip="true"
        label="【工序 │ 单价】"
        min-width="160px"
      >
        <template v-slot="scope">
            <span v-for="(item, index) in scope.row.processSequence" :key="item.processId">
            <span style="cursor: pointer" v-html="item.html" @click="crud.toEdit(item)" />
            <span v-if="index !== scope.row.processSequence.length - 1">→</span>
          </span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="创建时间" width="110px" />
      <!-- <el-table-column
        align="right"
        :label="`合计:${crud.query.sequenceType === typeEnum.ENCLOSURE.V ? '（元/米）' : '（元/吨）'}`"
        width="120"
        fixed="right"
      >
        <template v-slot="scope">
          <span>{{ scope.row.totalPrice }}</span>
        </template>
      </el-table-column> -->
      <!--编辑-->
      <!-- <el-table-column v-if="checkPermission([...permission.edit])" label="操作" width="90px" align="center" fixed="right">
        <template v-slot="scope">
          <udOperation :data="scope.row" :show-del="false" />
        </template>
      </el-table-column> -->
    </common-table>
    <!--分页组件-->
    <pagination />
    <mForm />
  </div>
</template>

<script setup>
import crudApi, { editWageQuota as edit } from '@/api/mes/production-config/product-process'
import { getMachinePart } from '@/api/mes/production-config/wage-quota'
import { ref } from 'vue'

import { isNotBlank } from '@data-type/index'
import { parseTime } from '@/utils/date'
import { deepClone } from '@/utils/data-type'
import { wageQuotaTypeEnum, processTypeEnum, processMaterialListTypeEnum as typeEnum } from '@enum-ms/mes'
// import checkPermission from '@/utils/system/check-permission'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
// import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'

// crud交由presenter持有
const permission = {
  get: ['wageQuota:get'],
  edit: ['wageQuota:edit']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '工价定额',
    sort: [],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi, edit }
  },
  tableRef
)

CRUD.HOOK.beforeToQuery = () => {
  crud.crudApi.get = crud.query.sequenceType === typeEnum.MACHINE_PART.V ? getMachinePart : crudApi.get
}

const { maxHeight } = useMaxHeight({ paginate: true })

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    if (crud.query.sequenceType === typeEnum.MACHINE_PART.V) {
      v.processName = v.name
      v.processId = v.id
      v.sequenceType = typeEnum.MACHINE_PART.V
      v.productProcessId = 0 // 零件没有总工序id
      v.process = [{ ...v, wageQuota: deepClone(v) }]
    } else {
      v.createTime = parseTime(v.createTime)
      v.process = deepClone(v.medBuildingProductProcessLinkList)
    }
    if (v.process && v.process.length > 0) {
      v.processOption = {}
      // v.totalPrice = 0
      v.process.forEach((o) => {
        v.processOption[o.processId] = {
          id: o.processId,
          name: o.processName,
          price: o.wageQuota.price,
          wageQuotaType: o.wageQuotaType,
          productProcessId: o.productProcessId
        }
        // v.totalPrice += o.wageQuota.price || 0
      })
      // v.processSequence = v.process
      //   .map((v) => {
      //     const unit = isNotBlank(v.wageQuotaType) ? wageQuotaTypeEnum.V[v.wageQuotaType].unit : ''
      //     return `<span>【${v.processName} │ <span style="color: #67C23A;">${
      //       isNotBlank(v.wageQuota?.price) ? v.wageQuota.price + ' ' : '0'
      //     }${unit}</span>】</span>`
      //   })
      //   .join('<span>→</span>')
      v.processSequence = v.process.map((o) => {
        const unit = isNotBlank(o.wageQuotaType) ? wageQuotaTypeEnum.V[o.wageQuotaType].unit : ''
        return {
          ...v,
          processId: o.processId,
          price: o.wageQuota?.price,
          unit: unit,
          html: `<span>【${o.processName} │ <span style="color: #67C23A;">${
            isNotBlank(o.wageQuota?.price) ? o.wageQuota.price + ' ' : '0'
          } ${unit}</span>】</span>`
        }
      })
      v.processSequenceIds = v.process.map((v) => v.processId)
    } else {
      v.processSequence = ''
      v.processSequenceIds = []
    }
    return v
  })
}
</script>
