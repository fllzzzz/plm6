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
        return-source-data
        :showEmptySymbol="false"
        style="width: 100%"
        @selection-change="crud.selectionChangeHandler"
        @sort-change="crud.handleSortChange"
      >
        <el-table-column key="selection" type="selection" width="55" :selectable="selectable"/>
        <el-table-column prop="index" label="序号" align="center" width="60" type="index" />
        <el-table-column
          v-if="columns.visible('monomer.name')"
          key="monomer.name"
          prop="monomer.name"
          :show-overflow-tooltip="true"
          label="单体"
          min-width="100px"
        />
        <el-table-column
          v-if="columns.visible('area.name')"
          key="area.name"
          prop="area.name"
          :show-overflow-tooltip="true"
          label="区域"
          min-width="100px"
        />
        <el-table-column
          v-if="columns.visible('name')"
          key="name"
          prop="name"
          :show-overflow-tooltip="true"
          label="名称"
          min-width="100px"
        />
        <el-table-column
          v-if="columns.visible('serialNumber')"
          key="serialNumber"
          prop="serialNumber"
          :show-overflow-tooltip="true"
          label="编号"
          min-width="140px"
        >
          <template v-slot="scope">
            <span>{{ scope.row.serialNumber }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('oldSpecification')"
          key="oldSpecification"
          prop="oldSpecification"
          :show-overflow-tooltip="true"
          :label="crud.query.boolAmendStatus?'旧规格':'规格'"
          min-width="120"
        />
        <el-table-column
          v-if="columns.visible('newSpecification')"
          key="newSpecification"
          prop="newSpecification"
          :show-overflow-tooltip="true"
          label="新规格"
          min-width="120"
        >
          <template v-slot="scope">
            <span>{{ scope.row.newSpecification || '-' }}</span>
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('length')"
          key="length"
          prop="length"
          :show-overflow-tooltip="true"
          label="长度(mm)"
          align="left"
          min-width="85px"
        >
          <template v-slot="scope">
            {{ scope.row.length ? scope.row.length.toFixed(DP.MES_ARTIFACT_L__MM) : '-' }}
          </template>
        </el-table-column>
        <el-table-column
          v-if="columns.visible('material')"
          key="material"
          prop="material"
          :show-overflow-tooltip="true"
          label="材质"
          align="left"
          min-width="80px"
        />
        <el-table-column
          v-if="columns.visible('quantity')"
          key="quantity"
          prop="quantity"
          label="清单数"
          align="left"
          min-width="80px"
        />
        <el-table-column
          v-if="columns.visible('netWeight')"
          key="netWeight"
          prop="netWeight"
          :show-overflow-tooltip="true"
          label="单净重(kg)"
          align="left"
          min-width="80px"
        >
          <template v-slot="scope">
            {{ scope.row.netWeight ? scope.row.netWeight.toFixed(DP.COM_WT__KG) : '-' }}
          </template>
        </el-table-column>
        <!--编辑与删除-->
        <el-table-column
          v-if="crud.query.boolAmendStatus===false"
          label="操作"
          width="130px"
          align="center"
          fixed="right"
        >
        <!-- <el-table-column
          v-if="checkPermission([...permission.edit])"
          label="操作"
          width="130px"
          align="center"
          fixed="right"
        > -->
          <template v-slot="scope">
            <!-- <udOperation :data="scope.row" :permission="permission" :show-del="false" :show-edit="!scope.row.newSpecPrefix"/> -->
            <udOperation :data="scope.row" :show-del="false" :show-edit="!scope.row.newSpecPrefix"/>
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <pagination />
      <mForm />
  </div>
</template>

<script setup>
import crudApi, { specConfig } from '@/api/mes/craft-manage/artifact-specification-revise'
import { ref, provide } from 'vue'

import useMaxHeight from '@compos/use-max-height'
import useCRUD from '@compos/use-crud'
import pagination from '@crud/Pagination'
// import { mapGetters } from '@/store/lib'
import { DP } from '@/settings/config'
import { artifactProductLineEnum } from '@enum-ms/mes'
// import { artifactPM as permission } from '@/page-permission/plan'

import udOperation from '@crud/UD.operation'
import mHeader from './module/header'
import mForm from './module/form'

// const { globalProjectId } = mapGetters(['globalProjectId'])

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()
const specList = ref([])
const { crud, columns } = useCRUD(
  {
    title: '构件规格修正',
    sort: ['id.asc'],
    // permission: { ...permission },
    optShow: { ...optShow },
    requiredQuery: ['projectId'],
    crudApi: { ...crudApi },
    hasPagination: true
  },
  tableRef
)

const { maxHeight } = useMaxHeight({
  wrapperBox: '.artifact-specification-revise',
  paginate: true,
  extraHeight: 40
})

// watch(
//   () => globalProjectId,
//   (val) => {
//     if (val) {
//       crud.query.projectId = globalProjectId
//       crud.toQuery()
//     }
//   },
//   { immediate: true, deep: true }
// )

getSpecConfig()

provide('specList', specList.value)

function selectable(row, rowIndex) {
  return !row.newSpecPrefix
}

async function getSpecConfig() {
  specList.value = []
  try {
    const { content } = await specConfig({ productionLineTypeEnum: artifactProductLineEnum.TRADITION.V })
    for (let i = 0; i < content.length; i++) {
      specList.value.push({
        id: content[i],
        value: content[i]
      })
    }
  } catch (e) {
    console.log('获取构件截面定义', e)
  }
}
</script>

<style lang="scss" scoped>
::v-deep(.abnormal-row) {
  background: #e8f4ff;
}
::v-deep(.hidden-select) {
  td:nth-child(1) {
    .cell {
      opacity: 0;
    }
  }
}
$font-size: 1.5em;
.child {
  width: $font-size;
  height: $font-size;
  display: inline-block;
  border: 1px solid;
  border-radius: 50%;
  line-height: $font-size;
}
</style>
