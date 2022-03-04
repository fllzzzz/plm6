<template>
  <div class="app-container">
    <!-- 工具栏 -->
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
      <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
      <el-table-column
        v-if="columns.visible('projectName')"
        key="projectName"
        prop="projectName"
        :show-overflow-tooltip="true"
        label="项目名称"
        min-width="60"
      >
        <template v-slot="scope">
          <span>{{ scope.row.projectName }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" key="areaName" prop="areaName" :show-overflow-tooltip="true" label="所选单体" min-width="80px">
        <template v-slot="scope">
          <span>{{ scope.row.areaName }}</span>
        </template>
      </el-table-column>
      <el-table-column align="left" key="thick" prop="thick" :show-overflow-tooltip="true" label="厚度（mm）" min-width="50">
        <template v-slot="scope">
          <span>{{ scope.row.thick }}</span>
          <el-tag style="float: right; margin-right: 10px" v-if="scope.row.relationType && scope.row.relationType === 2" type="success">
            零件板
          </el-tag>
          <el-tag
            style="float: right; margin-right: 10px"
            v-else-if="scope.row.relationType && scope.row.relationType === 16"
            type="danger"
          >
            翼腹板
          </el-tag>
        </template>
      </el-table-column>
      <el-table-column align="center" key="material" prop="material" :show-overflow-tooltip="true" label="材质" min-width="40">
        <template v-slot="scope">
          <span>{{ scope.row.material }}</span>
        </template>
      </el-table-column>
      <el-table-column align="right" key="sum" prop="sum" :show-overflow-tooltip="true" label="零件数量（件）" min-width="40">
        <template v-slot="scope">
          <span>{{ scope.row.sum }}</span>
        </template>
      </el-table-column>
      <el-table-column align="right" key="reduce" prop="reduce" :show-overflow-tooltip="true" label="零件重量（kg）" min-width="40">
        <template v-slot="scope">
          <span>{{ scope.row.reduce }}</span>
        </template>
      </el-table-column>
      <el-table-column align="right" key="totalNum" prop="totalNum" :show-overflow-tooltip="true" label="累计下发数（件）" min-width="40">
        <template v-slot="scope">
          <span>{{ scope.row.totalNum }}</span>
        </template>
      </el-table-column>
      <el-table-column
        align="right"
        key="totalWeight"
        prop="totalWeight"
        :show-overflow-tooltip="true"
        label="累计下发量（kg）"
        min-width="40"
      >
        <template v-slot="scope">
          <span>{{ scope.row.totalWeight }}</span>
        </template>
      </el-table-column>
      <el-table-column align="center" key="state" prop="state" :show-overflow-tooltip="true" label="状态" min-width="40">
        <template v-slot="scope">
          <span>
            <el-tag style="width: 100%" v-if="scope.row.state && scope.row.state === '2'" type="success">排套结束</el-tag>
            <el-tag style="width: 100%" v-else type="primary">未排套</el-tag>
          </span>
        </template>
      </el-table-column>
      <!-- <el-table-column v-if="columns.visible('updateTime')" key="updateTime" prop="updateTime" label="编辑日期" width="140px">
        <template v-slot="scope">
          <span>{{ parseTime(scope.row.updateTime) }}</span>
        </template>
      </el-table-column>
      <el-table-column v-if="columns.visible('createTime')" key="createTime" prop="createTime" label="创建日期" width="140px">
        <template v-slot="scope">
          <span>{{ parseTime(scope.row.createTime) }}</span>
        </template>
      </el-table-column> -->
      <el-table-column :show-overflow-tooltip="true" label="操作" align="center">
        <template v-slot="scope">
          <common-button icon="el-icon-view" @click="details(scope.row)" type="primary" size="mini">查 看</common-button>
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />

    <!-- 零件清单 -->
    <common-dialog width="50%" title="零件清单" append-to-body v-model="innerVisible">
      <common-table ref="tableRef" :data="PartByCutTaskIdData" :max-height="400" style="width: 100%" row-key="id">
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column key="monomerName" prop="monomerName" :show-overflow-tooltip="true" label="单体" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.monomerName }}</span>
          </template>
        </el-table-column>
        <el-table-column key="areaName" prop="areaName" :show-overflow-tooltip="true" label="区域" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.areaName }}</span>
          </template>
        </el-table-column>
        <el-table-column key="serialNumber" prop="serialNumber" :show-overflow-tooltip="true" label="编号" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column key="specification" prop="specification" :show-overflow-tooltip="true" label="规格" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.specification }}</span>
          </template>
        </el-table-column>
        <el-table-column key="length" prop="length" :show-overflow-tooltip="true" label="长度" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.length }}</span>
          </template>
        </el-table-column>
        <el-table-column key="material" prop="material" :show-overflow-tooltip="true" label="材质" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.material }}</span>
          </template>
        </el-table-column>
        <el-table-column key="quantity" prop="quantity" :show-overflow-tooltip="true" label="数量" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.quantity }}</span>
          </template>
        </el-table-column>
        <el-table-column key="netWeight" prop="netWeight" :show-overflow-tooltip="true" label="单重（kg）" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.netWeight }}</span>
          </template>
        </el-table-column>
        <el-table-column key="totalNetWeight" prop="totalNetWeight" :show-overflow-tooltip="true" label="总重（kg）" min-width="60">
          <template v-slot="scope">
            <span>{{ scope.row.totalNetWeight }}</span>
          </template>
        </el-table-column>
      </common-table>
    </common-dialog>
  </div>
</template>

<script setup>
import { ref } from 'vue'
import { getTaskPack, ByCutTaskId } from '@/api/cutting/radan-controller'
import useCRUD from '@compos/use-crud'
// import { parseTime } from '@/utils/date'
import mHeader from './module/header'
import pagination from '@crud/Pagination'
import useMaxHeight from '@compos/use-max-height'

const innerVisible = ref(false)
const PartByCutTaskIdData = ref([]) // 零件清单
const tableRef = ref()

// crud交由presenter持有
const permission = {
  get: ['contractRecord:get']
}

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const { crud, columns } = useCRUD(
  {
    title: '项目数据',
    sort: ['id.desc'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { get: getTaskPack },
    hasPagination: true,
    formStore: true,
    formStoreKey: 'BASE_CONFIG_UNIT'
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.contractRecord',
  paginate: true,
  extraHeight: 40
})

async function getByCutTaskId(params) {
  const { content } = await ByCutTaskId(params)
  PartByCutTaskIdData.value = content
}

function details(row) {
  innerVisible.value = true
  getByCutTaskId({ cutTaskId: row.cutTaskId })
}
</script>
