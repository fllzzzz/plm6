<template>
  <div>
    <div v-show="!lineId">
      <div class="my-code">点击生产线查看详情</div>
    </div>
    <div v-show="lineId">
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
        :max-height="maxHeight + 42"
        style="width: 100%"
      >
        <el-table-column label="序号" type="index" align="center" width="60" />
        <el-table-column
          v-if="columns.visible('processName')"
          key="processName"
          prop="processName"
          :show-overflow-tooltip="true"
          label="工序名称"
          width="120px"
        />
        <el-table-column
          v-if="columns.visible('organizationType')"
          key="organizationType"
          prop="organizationType"
          label="属性"
          width="100px"
        >
          <template v-slot="scope">
            {{ teamAttributeEnum.VL[scope.row.organizationType] }}
          </template>
        </el-table-column>
        <el-table-column v-if="columns.visible('leaderName')" key="leaderName" prop="leaderName" label="组长" width="100px" />
        <el-table-column
          v-if="columns.visible('memberNames')"
          key="memberNames"
          prop="memberNames"
          :show-overflow-tooltip="true"
          label="组员"
          min-width="160px"
        />
        <!--编辑与删除-->
        <el-table-column
          v-if="checkPermission([...permission.edit, ...permission.del])"
          label="操作"
          width="130px"
          align="center"
          fixed="right"
        >
          <template v-slot="scope">
            <udOperation :data="scope.row" />
          </template>
        </el-table-column>
      </common-table>
      <!--分页组件-->
      <pagination />
      <mForm :productType="line.productType" />
    </div>
  </div>
</template>

<script setup>
import crudApi from '@/api/mes/production-config/production-line-team'
import { defineExpose, ref, defineProps, watch, computed, inject } from 'vue'
import { teamAttributeEnum } from '@enum-ms/mes'
import checkPermission from '@/utils/system/check-permission'
import { configProductionLineTeamPM as permission } from '@/page-permission/config'

import useCRUD from '@compos/use-crud'
import udOperation from '@crud/UD.operation'
import pagination from '@crud/Pagination'
import mHeader from './module/header'
import mForm from './module/form'

const tableRef = ref()
const { crud, columns, CRUD } = useCRUD(
  {
    title: '班组',
    sort: [],
    permission: { ...permission },
    crudApi: { ...crudApi },
    queryOnPresenterCreated: false
  },
  tableRef
)

const maxHeight = inject('maxHeight')

const props = defineProps({
  line: {
    type: Object,
    default: () => {}
  }
})

const lineId = computed(() => {
  return props.line && props.line.id
})

watch(
  () => lineId,
  (val) => {
    if (val.value) {
      crud.toQuery()
    }
  },
  { deep: true, immediate: true }
)

CRUD.HOOK.beforeRefresh = () => {
  crud.query.productionLineId = lineId
  return !!crud.query.productionLineId
}

CRUD.HOOK.beforeToQuery = () => {
  crud.query.productionLineId = lineId
  return !!crud.query.productionLineId
}

CRUD.HOOK.handleRefresh = (crud, res) => {
  res.data.content = res.data.content.map((v) => {
    const members = []
    v.mesBuildingTeamUserLinkList.forEach((m) => {
      if (m.boolLeaderEnum) {
        v.leaderName = m.userName
        v.leaderId = m.userId
        v.leader = {
          id: m.userId,
          name: m.userName
        }
      } else {
        members.push({
          teamId: m.teamId,
          id: m.userId,
          name: m.userName
        })
      }
    })
    v.members = members
    if (v.members.length > 0) {
      v.memberNames = v.members.map((v) => v.name).join(', ')
      v.memberIds = v.members.map((v) => v.id)
    } else {
      v.memberNames = ''
      v.memberIds = []
    }
    return v
  })
}

CRUD.HOOK.beforeSubmit = () => {
  crud.form.productionLineId = lineId
  crud.form.factoryId = props.line.factoryId
  crud.form.workshopId = props.line.workshopId

  const members = crud.form.members
  const userList = []
  userList.push({
    boolLeaderEnum: true,
    userId: crud.form.leader.id,
    userName: crud.form.leader.name,
    teamId: crud.form.id
  })
  for (let i = 0; i < members.length; i++) {
    if (members[i]) {
      userList.push({
        boolLeaderEnum: false,
        userId: members[i].id,
        userName: members[i].name,
        teamId: crud.form.id
      })
    }
  }
  crud.form.userLinks = userList
}

defineExpose({
  permission,
  toAdd: crud.toAdd
})
</script>
