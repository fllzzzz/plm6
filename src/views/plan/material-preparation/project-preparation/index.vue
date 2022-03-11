<template>
  <div class="app-container">
    <!--工具栏-->
    <mHeader />
    <!-- 表格渲染 -->
    <common-table
      ref="tableRef"
      v-loading="!steelClassifyConfLoaded || crud.loading"
      :data="crud.data"
      :max-height="maxHeight"
      row-key="id"
    >
      <el-table-column label="序号" type="index" align="center" width="60" />
      <el-table-column
        v-if="columns.visible('serialNumber')"
        key="serialNumber"
        :show-overflow-tooltip="true"
        prop="serialNumber"
        label="备料单号"
        min-width="140px"
      />
      <el-table-column v-if="columns.visible('project')" show-overflow-tooltip key="project" prop="project" label="项目" min-width="150">
        <template #default="{ row }">
          <span v-parse-project="{ project: row.project, onlyShortName: true }" v-empty-text />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('monomer.name')"
        key="monomer.name"
        prop="monomer.name"
        :show-overflow-tooltip="true"
        label="单体"
        min-width="120"
      >
        <template #default="{ row }">
          <span v-empty="{ val: row.monomer ? row.monomer.name : void 0 }" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('area.name')"
        key="area.name"
        prop="area.name"
        :show-overflow-tooltip="true"
        label="区域"
        min-width="100"
      >
        <template #default="{ row }">
          <span v-empty="{ val: row.area ? row.area.name : void 0 }" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('technologyListType')"
        key="technologyListType"
        :show-overflow-tooltip="true"
        prop="technologyListType"
        label="清单类型"
        align="center"
        width="70"
      >
        <template #default="{ row }">
          <span v-parse-enum="{ e: componentTypeEnum, v: row.technologyListType }" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('materialBasicClass')"
        key="materialBasicClass"
        :show-overflow-tooltip="true"
        prop="materialBasicClass"
        label="物料种类"
        align="center"
        width="90"
      >
        <template #default="{ row }">
          <span v-parse-enum="{ e: projectPreparationMatClsEnum, v: row.materialBasicClass, bit: true }" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('listMete')"
        key="listMete"
        :show-overflow-tooltip="true"
        prop="listMete"
        label="清单量"
        align="center"
        width="110"
      >
        <template #default="{ row }">
          <template v-if="!(row.materialBasicClass & projectPreparationMatClsEnum.MATERIAL.V) && isNotBlank(row.listMete)">
            <span v-text="row.listMete" v-empty />
            <span>&nbsp;&nbsp;kg</span>
          </template>
          <span v-else v-empty />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('mete')"
        key="mete"
        :show-overflow-tooltip="true"
        prop="mete"
        label="备料量"
        align="center"
        width="110"
      >
        <template #default="{ row }">
          <template v-if="!(row.materialBasicClass & projectPreparationMatClsEnum.MATERIAL.V) && isNotBlank(row.preparationMete)">
            <span v-text="row.preparationMete" v-empty />
            <span>&nbsp;&nbsp;kg</span>
          </template>
          <span v-else v-empty />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('listUploaderNames')"
        key="listUploaderNames"
        :show-overflow-tooltip="true"
        prop="listUploaderNames"
        label="清单上传人"
        align="center"
        min-width="100"
      >
        <template #default="{ row }">
          <span v-split="row.listUploaderNames" v-empty />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('preparationUpdaterName')"
        key="preparationUpdaterName"
        :show-overflow-tooltip="true"
        prop="preparationUpdaterName"
        label="备料更新人"
        align="center"
        width="90"
      />
      <el-table-column
        v-if="columns.visible('createTime')"
        key="createTime"
        :show-overflow-tooltip="true"
        prop="createTime"
        label="创建日期"
        align="center"
        width="100"
      >
        <template #default="{ row }">
          <span v-parse-time="{ val: row.createTime, fmt: '{y}-{m}-{d}' }" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('listUpdateTime')"
        key="listUpdateTime"
        :show-overflow-tooltip="true"
        prop="listUpdateTime"
        label="清单更新时间"
        align="center"
        width="125"
      >
        <template #default="{ row }">
          <span v-parse-time="row.listUpdateTime" />
        </template>
      </el-table-column>
      <el-table-column
        v-if="columns.visible('preparationUpdateTime')"
        key="preparationUpdateTime"
        :show-overflow-tooltip="true"
        prop="preparationUpdateTime"
        label="备料更新时间"
        align="center"
        width="125"
      >
        <template #default="{ row }">
          <span v-parse-time="row.preparationUpdateTime" v-empty />
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column label="备料" width="105" align="center" fixed="right">
        <template #default="{ row }">
          <common-button
            v-if="checkPermission(permission.edit)"
            size="mini"
            :type="row.boolPrepared ? 'success' : 'warning'"
            plain
            @click.stop="crud.toEdit(row)"
          >
            {{ row.boolPrepared ? '再次备料' : '前往备料' }}
          </common-button>
          <template v-else>
            <el-tag v-if="row.boolPrepared" type="success">已备料</el-tag>
            <el-tag v-else type="warning">未备料</el-tag>
          </template>
        </template>
      </el-table-column>
      <!--编辑与删除-->
      <el-table-column label="操作" width="170px" align="center" fixed="right">
        <template #default="{ row }">
          <udOperation :show-del="false" :show-edit="false" :data="row" detail-icon="el-icon-tickets" :detail-type="null" show-detail />
          <common-button size="mini" icon="el-icon-time" type="info" @click="openLog(scope.row)" />
          <e-operation :data="row.id" :permission="permission.download" />
        </template>
      </el-table-column>
    </common-table>
    <!--分页组件-->
    <pagination />
    <!-- 表单 -->
    <m-form />
    <m-detail />
  </div>
</template>

<script setup>
import crudApi from '@/api/plan/material-preparation/project-preparation'
import { materialProjectPreparationPM as permission } from '@/page-permission/plan'

import { ref } from 'vue'
import { projectPreparationMatClsEnum } from '@enum-ms/classification'
import checkPermission from '@/utils/system/check-permission'
import { componentTypeEnum } from '@/utils/enum/modules/building-steel'
import { isNotBlank } from '@/utils/data-type'

import useCRUD from '@compos/use-crud'
import useMaxHeight from '@compos/use-max-height'
import pagination from '@crud/Pagination'
import eOperation from '@crud/E.operation'
import udOperation from '@crud/UD.operation.vue'
import mHeader from './module/header'
import mForm from './module/form'
import mDetail from './module/detail'

import useSteelClassifyConf from '@/composables/store/use-steel-material-classify'

const optShow = {
  add: false,
  edit: false,
  del: false,
  download: false
}

const tableRef = ref()

const { CRUD, crud, columns } = useCRUD(
  {
    title: '项目备料',
    sort: ['id.desc'],
    invisibleColumns: ['createTime'],
    permission: { ...permission },
    optShow: { ...optShow },
    crudApi: { ...crudApi }
  },
  tableRef
)

const { maxHeight } = useMaxHeight({ paginate: true })
// 获取钢材材料配置
const { loaded: steelClassifyConfLoaded } = useSteelClassifyConf((content) => {
  crud.props.steelClassifyConfICKV = content || {}
}, true)

CRUD.HOOK.handleRefresh = (crud, { data }) => {
  // data.content = data.content.map((v) => {
  //   return v
  // })
}

// 查看修改日志
function openLog() {}
</script>
